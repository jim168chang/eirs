%% @doc Some utility functions for converting common bit values
-module(bitutil).

%%
%% Exported Functions
%%
-export([boolean/1]).

%%
%% API Functions
%%

%% @spec boolean(SingleBit::binary()) -> bool()
%% @doc Returns an atom of 'true' if the bit is set, 'false' otherwise (without the single quotes)
boolean(2#0) -> false;
boolean(2#1) -> true.
