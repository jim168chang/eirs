
%% @type id3v2_header().
%%      <dl>
%%          <dt>major_version</dt><dd>integer()</dd>
%%          <dt>minor_version</dt><dd>integer()</dd>
%%          <dt>unsynchronization</dt><dd>bool()</dd>
%%          <dt>extended_header</dt><dd>bool()</dd>
%%          <dt>experimental</dt><dd>bool()</dd>
%%          <dt>custom_flags</dt><dd>integer()</dd>
%%          <dt>length</dt><dd>integer()</dd>
%%      </dl>
-record(id3v2_header, { major_version, minor_version, unsynchronization, extended_header, experimental, custom_flags, length }).

%% @type id3v2_tag().
%%      <dl>
%%          <dt>header</dt><dd>id3v2_header()</dd>
%%          <dt>tag_data</dt><dd>binary()</dd>
%%      </dl>
-record(id3v2_tag, { header, tag_data }).

%% @type mpeg_audio_frame_header().
%%      <dl>
%%          <dt>mpeg_version</dt><dd>1 | 2 | 2.5</dd>
%%          <dt>layer</dt><dd>1 | 2 | 3</dd>
%%          <dt>protected</dt><dd>bool()</dd>
%%          <dt>bit_rate</dt><dd>free_format | integer() [in kbps]</dd>
%%          <dt>sample_rate</dt><dd>integer() [in Hz]</dd>
%%          <dt>padded</dt><dd>bool()</dd>
%%          <dt>private</dt><dd>bool()</dd>
%%          <dt>channel_mode</dt><dd>stereo | joint_stereo | dual_channel | single_channel</dd>
%%          <dt>mode_extension</dt><dd>{subbands, {4,31}} | {subbands, {8,31}} | {subbands, {12,31}} | {subbands, {16,31}} | {{intensity_stereo, bool()}, {ms_stereo, bool()}}</dd>
%%          <dt>copyrighted</dt><dd>bool()</dd>
%%          <dt>original</dt><dd>bool()</dd>
%%          <dt>emphasis</dt><dd>none | fifty_over_fifteen | ccit_j17</dd>
%%          <dt>length</dt><dd>integer()</dd>
%%      </dl>
-record(mpeg_audio_frame_header, { mpeg_version, layer, protected, bit_rate, sample_rate, padded, private, channel_mode, mode_extension, copyrighted, original, emphasis, length }).

%% @type xing_header().
%%      <dl>
%%          <dt>header_id</dt><dd>atom() -> xing | info</dd>
%%          <dt>frames</dt><dd>integer() | not_present</dd>
%%          <dt>bytes</dt><dd>integer() | not_present</dd>
%%          <dt>seek_points</dt><dd>[integer()] | not_present</dd>
%%          <dt>quality_indicator</dt><dd>integer() | not_present</dd>
%%      </dl>
-record(xing_header, { header_id, frames, bytes, seek_points, quality_indicator }).

%% @type vbri_header().
%%      <dl>
%%          <dt>version_id</dt><dd>integer()</dd>
%%          <dt>delay</dt><dd>integer()</dd>
%%          <dt>quality_indicator</dt><dd>integer()</dd>
%%          <dt>bytes</dt><dd>integer()</dd>
%%          <dt>frames</dt><dd>integer()</dd>
%%          <dt>toc_entry_count</dt><dd>integer()</dd>
%%          <dt>toc_scale_factor</dt><dd>integer()</dd>
%%          <dt>toc_entry_size</dt><dd>integer()</dd>
%%          <dt>frames_per_entry</dt><dd>integer()</dd>
%%      </dl>
-record(vbri_header, { version_id, delay, quality_indicator, bytes, frames, toc_entry_count, toc_scale_factor, toc_entry_size, frames_per_entry }).

%% @type mpeg_audio_frame().
%%      <dl>
%%          <dt>header</dt><dd>mpeg_audio_frame_header()</dd>
%%          <dt>xing</dt><dd>xing_header() | not_present</dd>
%%          <dt>vbri</dt><dd>vbri_header() | not_present</dd>
%%          <dt>audio_data</dt><dd>binary()</dd>
%%      </dl>
-record(mpeg_audio_frame, { header, xing, vbri, audio_data }).
