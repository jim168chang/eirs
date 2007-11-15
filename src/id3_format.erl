%% @doc Decodes ID3 (v2 only right now) blocks by parsing and providing information about ID3 frames
-module(id3_format).

%%
%% Imported Functions
%%
-import(bitutil, [boolean/1]).
-include("audio_records.hrl").

%%
%% Exported Functions
%%
-export([id3v2_tag/1, id3v2_header/1]).

%%
%% API Functions
%%

%% @spec id3v2_tag(Id3v2Bitstream:binary()) -> {ok, id3v2_tag(), RemainingData:binary()} | id3v2_tag_not_found
%% @doc  Parses the the ID3v2 tag at the current bitstream position.  This function does NOT seek into the stream
%%       to try to find a tag, it assumes the tag is at the beginning.
id3v2_tag(Id3v2Bitstream) ->
    case catch(id3v2_header(Id3v2Bitstream)) of
        {ok, Id3v2Header} ->
            {<<_Header:80, TagData/binary>>, RemainingBitstream} = split_binary(Id3v2Bitstream, Id3v2Header#id3v2_header.length + 10),
            Id3v2Tag = #id3v2_tag {
                header   = Id3v2Header,
                tag_data = TagData
            },
            {ok, Id3v2Tag, RemainingBitstream};
        _ ->
            id3v2_tag_not_found
    end.

%% @spec id3v2_header(ID3Bitstream::binary()) -> {ok, id3v2_header()} | throw(bad_sync) | throw(bad_major_version) | throw(bad_minor_version)
%% @doc Decodes an ID3v2 header into a usable structure, assuming that the binary is already positioned at a valid ID3v2 header and is 10 bytes long
%% @throws bad_sync | bad_major_version | bad_minor_version
id3v2_header(<<$I, $D, $3, 255, _/binary>>)    -> throw(bad_major_version);
id3v2_header(<<$I, $D, $3, _, 255, _/binary>>) -> throw(bad_minor_version);
id3v2_header(<<$I, $D, $3, MajorVersion:8, MinorVersion:8, UnsynchronizationBit:1, ExtendedHeaderBit:1, ExperimentalBit:1, CustomFlagBits:5, 2#0:1, Length1:7, 2#0:1, Length2:7, 2#0:1, Length3:7, 2#0:1, Length4:7, _/binary>>) ->
    <<Length:32>> = <<2#0000:4, Length1:7, Length2:7, Length3:7, Length4:7>>,
    Id3v2Header = #id3v2_header {
        major_version     = MajorVersion,
        minor_version     = MinorVersion,
        unsynchronization = bitutil:boolean(UnsynchronizationBit),
        extended_header   = bitutil:boolean(ExtendedHeaderBit),
        experimental      = bitutil:boolean(ExperimentalBit),
        custom_flags      = CustomFlagBits,
        length            = Length
    },
    {ok, Id3v2Header};
id3v2_header(_) -> throw(bad_sync).

%%
%% Local Functions
%%

