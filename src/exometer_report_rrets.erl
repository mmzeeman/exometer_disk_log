%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2014 Maas-Maarten Zeeman
%%
%% @doc exometer_rrets, a rrets based persistency layer for exometer.
%%
%% Copyright 2014 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% Erlang diff-match-patch implementation

-module(exometer_report_rrets).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(exometer_report).

%% exometer_report callback API
-export([
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_terminate/2,
    exometer_newentry/2,
    exometer_setopts/4
]).

-include_lib("exometer/include/exometer.hrl").

-record(state, {
    storage :: rrets:storage()
}).

exometer_init(Opts) ->
    RRetsArgs = get_opt(rrets_args, Opts),

    case rrets:open(RRetsArgs) of
        {error, _Reason}=Error ->
            lager:error("Error triggered while opening. Error: ~p.", [Error]),
            Error;
        {ok, Storage} -> 
            {ok, #state{storage=Storage}}
    end.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

exometer_report(Metric, DataPoint, _Extra, Value, #state{storage=Storage}=State) ->
    Entry = {Metric, DataPoint, Value},
    rrets:log(Storage, Entry),
    {ok, State}.

exometer_call(info, _From, #state{storage=Storage}=State) ->
    Info = rrets:info(Storage),
    {reply, Info, State};
exometer_call(Unknown, From, State) ->
    lager:info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, State}.

exometer_cast(sync, #state{storage=Storage}=State) ->
    rrets:sync(Storage),
    {ok, State};
exometer_cast(Unknown, State) ->
    lager:info("Unknown cast: ~p", [Unknown]),
    {ok, State}.

exometer_info(Unknown, State) ->
    lager:info("Unknown info: ~p", [Unknown]),
    {ok, State}.

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_, #state{storage=Storage}) ->
    ok = rrets:close(Storage).

%%
%% Helpers
%%

get_opt(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} -> V;
        false  -> error({required, K})
    end.

