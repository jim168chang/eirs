%% @doc Decodes MPEG audio bitstreams by parsing and providing information about individual frames.
%%      Does not actually decode the audio frames into PCM samples, it only parses the frames :(
-module(mpeg_audio_format).

%%
%% Imported Functions
%%
-import(bitutil, [boolean/1]).
-include("audio_records.hrl").

%%
%% Exported Functions
%%
-export([mpeg_audio_frame/1, mpeg_audio_frame_header/1]).

%%
%% API Functions
%%

%% @spec mpeg_audio_frame(MpegAudioBitstrem:binary()) -> {ok, mpeg_audio_frame(), RemainingData:binary()} | mpeg_audio_frame_not_found
%% @doc  Parses the next available MPEG audio frame in the bitstream, possibly seeking into it if the stream is not
%%       positioned at a valid MPEG audio frame.
mpeg_audio_frame(MpegAudioBitstream) when size(MpegAudioBitstream) >= 4 ->
    case catch(mpeg_audio_frame_header(MpegAudioBitstream)) of
        {ok, MpegAudioHeader} ->
            {FrameBits, RemainingBitstream} = split_binary(MpegAudioBitstream, MpegAudioHeader#mpeg_audio_frame_header.length),
            {FrameHeaderBits, FrameData} = split_binary(FrameBits, 4),
            {ok, FrameHeader} = mpeg_audio_frame_header(FrameHeaderBits),
            Frame = #mpeg_audio_frame {
                header     = FrameHeader,
                xing       = xing(FrameData),
                vbri       = vbri_header(FrameData),
                audio_data = FrameData
            },
            {ok, Frame, RemainingBitstream};
        _ ->
            {_SkipByte, RemainingBitstream} = split_binary(MpegAudioBitstream, 1),
            mpeg_audio_frame(RemainingBitstream)
    end;
mpeg_audio_frame(_) -> mpeg_audio_frame_not_found.

%% @spec mpeg_audio_frame_header(MpegAudioBitstream::binary()) -> {ok, mpeg_audio_frame_header()} | throw(bad_sync_word) | throw(undefined_mpeg_version) | throw(undefined_mpeg_layer) | throw(bad_bitrate) | throw(bad_samplerate) | throw(bad_emphasis) | throw(invalid_settings)
%% @doc Decodes an MPEG audio frame header into a usable structure, assuming that the binary is already positioned at a valid frame header
%% @throws bad_sync_word | undefined_mpeg_version | undefined_mpeg_layer | bad_bitrate | bad_samplerate | bad_emphasis | invalid_settings
mpeg_audio_frame_header(<<2#11111111111:11,VersionBits:2,LayerBits:2,ProtectedBit:1,BitrateBits:4,SamplerateBits:2,PaddingBit:1,PrivateBit:1,ChannelModeBits:2,ModeExtBits:2,CopyrightBit:1,OriginalBit:1,EmphasisBits:2,_/binary>>) ->
    FrameHeader = #mpeg_audio_frame_header{
        mpeg_version   = mpeg_version(VersionBits),
        layer          = mpeg_layer(LayerBits),
        protected      = boolean(ProtectedBit),
        bit_rate       = bitrate_kbps(mpeg_version(VersionBits), mpeg_layer(LayerBits), BitrateBits),
        sample_rate    = samplerate(mpeg_version(VersionBits), SamplerateBits),
        padded         = bitutil:boolean(PaddingBit),
        private        = bitutil:boolean(PrivateBit),
        channel_mode   = channelmode(ChannelModeBits),
        mode_extension = modeextension(mpeg_layer(LayerBits), ModeExtBits),
        copyrighted    = bitutil:boolean(CopyrightBit),
        original       = bitutil:boolean(OriginalBit),
        emphasis       = emphasis(EmphasisBits),
        length         = framelength(mpeg_version(VersionBits), mpeg_layer(LayerBits), bitrate_kbps(mpeg_version(VersionBits), mpeg_layer(LayerBits), BitrateBits), samplerate(mpeg_version(VersionBits), SamplerateBits), PaddingBit)
    },
    validate_frame_header(FrameHeader);
mpeg_audio_frame_header(_) -> throw(bad_sync_word).

%%
%% Local Functions
%%

mpeg_version(2#00) -> 2.5;
mpeg_version(2#01) -> throw(undefined_mpeg_version);
mpeg_version(2#10) -> 2;
mpeg_version(2#11) -> 1.

mpeg_layer(2#00) -> throw(undefined_mpeg_layer);
mpeg_layer(2#01) -> 3;
mpeg_layer(2#10) -> 2;
mpeg_layer(2#11) -> 1.

bitrate_kbps_table() ->
    {
        {   % MPEG 1
            { 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448 },  % Layer 1
            { 32, 48, 56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384 },  % Layer 2
            { 32, 40, 48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320 }   % Layer 3
        },
        {   % MPEG 2/2.5
            { 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256 }, % Layer 1
            {  8, 16, 24, 32, 40, 48,  56,  64,  80,  96, 112, 128, 144, 160 }, % Layer 2
            {  8, 16, 24, 32, 40, 48,  56,  64,  80,  96, 112, 128, 144, 160 }  % Layer 3, same as 2
        }
    }.
bitrate_kbps(_, _, 2#0000) -> free_format;
bitrate_kbps(_, _, 2#1111) -> throw(bad_bitrate);
bitrate_kbps(2.5, Layer, BitrateBits) -> bitrate_kbps(2, Layer, BitrateBits);
bitrate_kbps(Version, Layer, BitrateBits) -> element(BitrateBits, element(Layer, element(Version, bitrate_kbps_table()))).

samplerate(_,   2#11) -> throw(bad_samplerate);
samplerate(1,   2#00) -> 44100;
samplerate(1,   2#01) -> 48000;
samplerate(1,   2#10) -> 32000;
samplerate(2,   2#00) -> 22050;
samplerate(2,   2#01) -> 24000;
samplerate(2,   2#10) -> 16000;
samplerate(2.5, 2#00) -> 11025;
samplerate(2.5, 2#01) -> 12000;
samplerate(2.5, 2#10) ->  8000.

channelmode(2#00) -> stereo;
channelmode(2#01) -> joint_stereo;
channelmode(2#10) -> dual_channel;
channelmode(2#11) -> single_channel.

modeextension(1, ModeExtBits) -> modeextension(2, ModeExtBits);
modeextension(2, 2#00) -> {subbands,  {4,31}};
modeextension(2, 2#01) -> {subbands,  {8,31}};
modeextension(2, 2#10) -> {subbands, {12,31}};
modeextension(2, 2#11) -> {subbands, {16,31}};
modeextension(3, 2#00) -> {{intensity_stereo, false}, {ms_stereo, false}};
modeextension(3, 2#01) -> {{intensity_stereo,  true}, {ms_stereo, false}};
modeextension(3, 2#10) -> {{intensity_stereo, false}, {ms_stereo,  true}};
modeextension(3, 2#11) -> {{intensity_stereo,  true}, {ms_stereo,  true}}.

emphasis(2#00) -> none;
emphasis(2#01) -> fifty_over_fifteen;
emphasis(2#10) -> throw(bad_emphasis);
emphasis(2#11) -> ccit_j17.

%% @spec framelength(Version:integer(), Layer:integer(), BitrateKbps:integer(), SampleRate:integer(), Padding:integer()) -> integer()
framelength(_, 1, BitRateKbps, SampleRate, Padding) -> trunc((12  * BitRateKbps * 1000 / SampleRate) + Padding) * 4;
framelength(2, 3, BitRateKbps, SampleRate, Padding) -> trunc((72  * BitRateKbps * 1000 / SampleRate) + Padding);
framelength(_, _, BitRateKbps, SampleRate, Padding) -> trunc((144 * BitRateKbps * 1000 / SampleRate) + Padding).

%% @spec validate_frame_header(mpeg_audio_frame_header()) -> {ok, mpeg_audio_frame_header()} | throw(invalid_settings)
%% @doc  assumes that the passed in frame header has already passed basic sanity checks, this function
%%       validates that all of the settings are legal: http://mpgedit.org/mpgedit/mpeg_format/MP3Format.html
%%       lists combinations of bit rate and mode which are disallowed for layer II formats.
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=224, channel_mode=single_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=256, channel_mode=single_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=320, channel_mode=single_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=384, channel_mode=single_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=32, channel_mode=stereo}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=48, channel_mode=stereo}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=56, channel_mode=stereo}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=80, channel_mode=stereo}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=32, mode_extension={{intensity_stereo, true}, _}}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=48, mode_extension={{intensity_stereo, true}, _}}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=56, mode_extension={{intensity_stereo, true}, _}}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=80, mode_extension={{intensity_stereo, true}, _}}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=32, channel_mode=dual_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=48, channel_mode=dual_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=56, channel_mode=dual_channel}) -> throw(invalid_settings);
validate_frame_header(#mpeg_audio_frame_header{layer=2, bit_rate=80, channel_mode=dual_channel}) -> throw(invalid_settings);
validate_frame_header(FrameHeader) -> {ok, FrameHeader}.

xing(<<$I, $n, $f, $o, RemainingBitstream/binary>>) ->
    xing(info, RemainingBitstream);
xing(<<$X, $i, $n, $g, RemainingBitstream/binary>>) ->
    xing(xing, RemainingBitstream);
xing(Bitstream) when size(Bitstream) > 4 ->
    {_FirstByte, RemainingBitstream} = split_binary(Bitstream, 1),
    xing(RemainingBitstream);
xing(_) -> not_present.

xing(VbrHeaderId, <<_Ignore:28, FramesFieldBit:1, BytesFieldBit:1, TOCFieldBit:1, QualityIndicatorFieldBit:1, XingFieldBitstream/binary>>) ->
    {FramesField, AfterFramesBitstream} = xing_int(FramesFieldBit, XingFieldBitstream),
    {BytesField,  AfterBytesBitstream}  = xing_int(BytesFieldBit,  AfterFramesBitstream),
    {TOCField,    AfterTOCBitstream}    = xing_toc(TOCFieldBit,    AfterBytesBitstream),
    {QualityIndicatorField, _}          = xing_int(QualityIndicatorFieldBit, AfterTOCBitstream),
    #xing_header {
        header_id         = VbrHeaderId,
        frames            = FramesField,
        bytes             = BytesField,
        seek_points       = TOCField,
        quality_indicator = QualityIndicatorField
    }.

xing_int(0, XingFieldBitstream) -> {not_present, XingFieldBitstream};
xing_int(1, <<FieldValue:32, RemainingXingBitstream/binary>>) -> {FieldValue, RemainingXingBitstream}.

xing_toc(0, XingFieldBitstream) -> {not_present, XingFieldBitstream};
xing_toc(1, XingFieldBitstream) ->
    {TOC, RemainingXingBitstream} = split_binary(XingFieldBitstream, 100),
    {binary_to_list(TOC), RemainingXingBitstream}.

vbri_header(<<$V, $B, $R, $I, VersionId:16/integer, Delay:16/integer, QualityIndicator:16/integer, Bytes:32/integer, Frames:32/integer, NumTOCEntries:16/integer, TOCScaleFactor:16/integer, TableEntrySize:16/integer, FramesPerTableEntry:16/integer, _/binary>>) ->
    #vbri_header {
        version_id        = VersionId,
        delay             = Delay,
        quality_indicator = QualityIndicator,
        bytes             = Bytes,
        frames            = Frames,
        toc_entry_count   = NumTOCEntries,
        toc_scale_factor  = TOCScaleFactor,
        toc_entry_size    = TableEntrySize,
        frames_per_entry  = FramesPerTableEntry
    };
vbri_header(Bitstream) when size(Bitstream) > 26 ->
    {_FirstByte, RemainingBitstream} = split_binary(Bitstream, 1),
    vbri_header(RemainingBitstream);
vbri_header(_) -> not_present.

