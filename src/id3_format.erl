%% @doc Decodes ID3 (v2 only right now) blocks by parsing the (non-extended) header and providing the raw ID3 frames, which includes any extended header data.
-module(id3_format).

%%
%% Imported Functions
%%
-import(lists, [reverse/1]).
-import(bitutil, [boolean/1]).
-include("audio_records.hrl").

%%
%% Exported Functions
%%
-export([id3_tag/1]).

%%
%% API Functions
%%

%% @spec id3_tag(Id3v23Bitstream:binary()) -> {id3v2_2 | id3v2_3 | id3v2_4, id3v2_tag(), RemainingBitstream:binary()} | id3_tag_not_found
%% @doc  Parses the ID3 tag at the current bitstream position.  This function does NOT seek into the stream, and currently supports ID3 version 2.2, 2.3 and 2.4 tags.
id3_tag(<<$I, $D, $3, 255, _/binary>>)    -> id3_tag_not_found;
id3_tag(<<$I, $D, $3, _, 255, _/binary>>) -> id3_tag_not_found;
id3_tag(<<$I, $D, $3, 2, HeaderRemainder:48, Bitstream/binary>>) ->
    try id3v22_header(<<$I, $D, $3, 2, HeaderRemainder:48>>) of
        {ok, Id3v22Header} ->
            case Id3v22Header#id3v22_header.compression of
                true ->
                    {id3v2_2, #id3_tag{ header = Id3v22Header, frame_data = unknown }, Bitstream};
                false ->
                    {<<Id3Frames/binary>>, RemainingBitstream} = id3v2_frames(Id3v22Header#id3v22_header.unsynchronization, Bitstream, Id3v22Header#id3v22_header.length),
                    {id3v2_2, #id3_tag{ header = Id3v22Header, frame_data = Id3Frames }, RemainingBitstream}
            end
    catch
        _:_ ->
            id3_tag_not_found
    end;
id3_tag(<<$I, $D, $3, 3, HeaderRemainder:48, Bitstream/binary>>) ->
    try id3v23_header(<<$I, $D, $3, 3, HeaderRemainder:48>>) of
        {ok, Id3v23Header} ->
            {<<Id3Frames/binary>>, RemainingBitstream} = id3v2_frames(Id3v23Header#id3v23_header.unsynchronization, Bitstream, Id3v23Header#id3v23_header.length),
            {id3v2_3, #id3_tag{ header = Id3v23Header, frame_data = Id3Frames }, RemainingBitstream}
    catch
        _ ->
            io:format("Caught exception~n", []),
            % io:format("Caught exception when matching ~p bytes~n", [size(<<$I, $D, $3, 3, HeaderRemainder>>)]),
            id3_tag_not_found
    end;
id3_tag(<<$I, $D, $3, 4, HeaderRemainder:48/binary, Bitstream/binary>>) ->
    try id3v24_header(<<$I, $D, $3, 4, HeaderRemainder:48>>) of
        {ok, Id3v24Header} ->
            {<<Id3Frames/binary>>, RemainingBitstream} = id3v2_frames(Id3v24Header#id3v24_header.unsynchronization, Bitstream, Id3v24Header#id3v24_header.length),
            case Id3v24Header#id3v24_header.footer_present of
                true ->
                    % The footer should be identical to the header except the leading ID3 should instead read 3DI, necessary to validate/strip but not to return
                    try split_binary(RemainingBitstream, 10) of
                        {<<$3, $D, $I, 4, HeaderRemainder>>, BitstreamAfterFooter} ->
                            {id3v2_4, #id3_tag{ header = Id3v24Header, frame_data = Id3Frames }, BitstreamAfterFooter}
                    catch
                        _ ->
                            id3_tag_not_found
                    end;
                false ->
                    {id3v2_4, #id3_tag{ header = Id3v24Header, frame_data = Id3Frames }, RemainingBitstream}
            end
    catch
        _:_ ->
            id3_tag_not_found
    end;
id3_tag(_) ->
    id3_tag_not_found.

%%
%% Local Functions
%%

%% @spec id3v22_header(ID3Bitstream::binary()) -> {ok, id3v22_header()}
%% @doc Decodes an ID3 2.2 header into a usable structure, assuming that the binary is already positioned at one
id3v22_header(<<$I, $D, $3, 2, MinorVersion:8, UnsynchronizationBit:1, CompressionBit:1, 2#000000:6, 2#0:1, Length1:7, 2#0:1, Length2:7, 2#0:1, Length3:7, 2#0:1, Length4:7, _/binary>>) ->
    <<Length:32>> = <<2#0000:4, Length1:7, Length2:7, Length3:7, Length4:7>>,
    Id3v22Header = #id3v22_header {
        minor_version     = MinorVersion,
        unsynchronization = bitutil:boolean(UnsynchronizationBit),
        compression       = bitutil:boolean(CompressionBit),
        length            = Length
    },
    {ok, Id3v22Header}.

%% @spec id3v23_header(ID3Bitstream::binary()) -> {ok, id3v23_header()}
%% @doc Decodes an ID3 2.3 header into a usable structure, assuming that the binary is already positioned at one
id3v23_header(<<$I, $D, $3, 3, MinorVersion:8, UnsynchronizationBit:1, ExtendedHeaderBit:1, ExperimentalBit:1, CustomFlagBits:5, 2#0:1, Length1:7, 2#0:1, Length2:7, 2#0:1, Length3:7, 2#0:1, Length4:7, _/binary>>) ->
    <<Length:32>> = <<2#0000:4, Length1:7, Length2:7, Length3:7, Length4:7>>,
    Id3v23Header = #id3v23_header {
        minor_version     = MinorVersion,
        unsynchronization = bitutil:boolean(UnsynchronizationBit),
        extended_header   = bitutil:boolean(ExtendedHeaderBit),
        experimental      = bitutil:boolean(ExperimentalBit),
        custom_flags      = CustomFlagBits,
        length            = Length
    },
    {ok, Id3v23Header}.

%% @spec id3v24_header(ID3Bitstream::binary()) -> {ok, id3v24_header()}
%% @doc Decodes an ID3 2.4 header into a usable structure, assuming that the binary is already positioned at one
id3v24_header(<<$I, $D, $3, 4, MinorVersion:8, UnsynchronizationBit:1, ExtendedHeaderBit:1, ExperimentalBit:1, FooterPresentBit:1, 2#0000:4, 2#0:1, Length1:7, 2#0:1, Length2:7, 2#0:1, Length3:7, 2#0:1, Length4:7, _/binary>>) ->
    <<Length:32>> = <<2#0000:4, Length1:7, Length2:7, Length3:7, Length4:7>>,
    Id3v24Header = #id3v24_header {
        minor_version     = MinorVersion,
        unsynchronization = bitutil:boolean(UnsynchronizationBit),
        extended_header   = bitutil:boolean(ExtendedHeaderBit),
        experimental      = bitutil:boolean(ExperimentalBit),
        footer_present    = bitutil:boolean(FooterPresentBit),
        length            = Length
    },
    {ok, Id3v24Header}.

%% @spec id3_frames(Synchronize:bool(), Bitstream:binary(), Count:integer()) -> {ID3Frames:binary(), RemainingBitstream:binary()}
%% @doc  Synchronizes (if necessary) an ID3 bitstream for the requested number of bytes.
id3v2_frames(false, Bitstream, Count) ->
    split_binary(Bitstream, Count);
id3v2_frames(true, Bitstream, Count) ->
    synchronize_acc(Bitstream, Count, []).

synchronize_acc(Bitstream, 0, SynchronizedByteAcc) -> { list_to_binary(lists:reverse(SynchronizedByteAcc)), Bitstream};
synchronize_acc(<<255, 0, RemainingBitstream/binary>>, Count, SynchronizedByteAcc) ->
    synchronize_acc(RemainingBitstream, Count - 1, [255|SynchronizedByteAcc]);
synchronize_acc(<<NormalByte:8/integer, RemainingBitstream/binary>>, Count, SynchronizedByteAcc) ->
    synchronize_acc(RemainingBitstream, Count - 1, [NormalByte|SynchronizedByteAcc]).
