%% @doc Decodes MP3 bitstreams by parsing and providing information about MP3 frames
-module(mp3_format).

%%
%% Exported Functions
%%
-export([mp3_frame_header/1]).

%%
%% Types / records
%%

%% @type mp3_frame_header().
%%      <dl>
%%          <dt>mpeg_version</dt><dd>1 | 2 | 2.5</dd>
%%          <dt>layer</dt><dd>1 | 2 | 3</dd>
%%          <dt>protected</dt><dd>bool()</dd>
%%          <dt>bit_rate</dt><dd>free_format | integer() [in bps]</dd>
%%          <dt>sample_rate</dt><dd>integer() [in Hz]</dd>
%%          <dt>padded</dt><dd>bool()</dd>
%%          <dt>private</dt><dd>bool()</dd>
%%          <dt>channel_mode</dt><dd>stereo | joint_stereo | dual_channel | single_channel</dd>
%%          <dt>mode_extension</dt><dd>{subbands, {4,31}} | {subbands, {8,31}} | {subbands, {12,31}} | {subbands, {16,31}} | {{intensity_stereo, bool()}, {ms_stereo, bool()}}</dd>
%%          <dt>copyrighted</dt><dd>bool()</dd>
%%          <dt>original</dt><dd>bool()</dd>
%%          <dt>emphasis</dt><dd>none | fifty_over_fifteen | ccit_j17</dd>
%%      </dl>
-record(mp3_frame_header, { mpeg_version, layer, protected, bit_rate, sample_rate, padded, private, channel_mode, mode_extension, copyrighted, original, emphasis }).

%%
%% API Functions
%%

%% @spec mp3_frame_header(Mp3Bitstream::binary()) -> {ok, mp3_frame_header()} | throw(undefined_mpeg_version) | throw(undefined_mpeg_layer) | throw(bad_bitrate) | throw(bad_samplerate) | throw(bad_emphasis)
%% @doc Decodes an MP3 frame header into a usable structure, assuming that the binary is already positioned at a valid MP3 header
%% @throws undefined_mpeg_version | undefined_mpeg_layer | bad_bitrate | bad_samplerate | bad_emphasis
mp3_frame_header(<<2#11111111111:11,VersionBits:2,LayerBits:2,ProtectedBit:1,BitrateBits:4,SamplerateBits:2,PaddingBit:1,PrivateBit:1,ChannelModeBits:2,ModeExtBits:2,CopyrightBit:1,OriginalBit:1,EmphasisBits:2>>) ->
    {ok, #mp3_frame_header{
                            mpeg_version   = mpeg_version(VersionBits),
                            layer          = mpeg_layer(LayerBits),
                            protected      = boolean(ProtectedBit),
                            bit_rate       = bitrate(mpeg_version(VersionBits), mpeg_layer(LayerBits), BitrateBits),
                            sample_rate    = samplerate(mpeg_version(VersionBits), SamplerateBits),
                            padded         = boolean(PaddingBit),
                            private        = boolean(PrivateBit),
                            channel_mode   = channelmode(ChannelModeBits),
                            mode_extension = modeextension(mpeg_layer(LayerBits), ModeExtBits),
                            copyrighted    = boolean(CopyrightBit),
                            original       = boolean(OriginalBit),
                            emphasis       = emphasis(EmphasisBits)
                           }}.

%%
%% Local Functions
%%

boolean(2#0) -> false;
boolean(2#1) -> true.

mpeg_version(2#00) -> 2.5;
mpeg_version(2#01) -> throw(undefined_mpeg_version);
mpeg_version(2#10) -> 2;
mpeg_version(2#11) -> 1.

mpeg_layer(2#00) -> throw(undefined_mpeg_layer);
mpeg_layer(2#01) -> 3;
mpeg_layer(2#10) -> 2;
mpeg_layer(2#11) -> 1.

bitrate_table() ->
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
bitrate(_, _, 2#0000) -> free_format;
bitrate(_, _, 2#1111) -> throw(bad_bitrate);
bitrate(2.5, Layer, BitrateBits) -> bitrate(2, Layer, BitrateBits);
bitrate(Version, Layer, BitrateBits) -> element(BitrateBits, element(Layer, element(Version, bitrate_table()))) * 1000.

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