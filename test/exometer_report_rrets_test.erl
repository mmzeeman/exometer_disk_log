
-module(exometer_report_rrets_test).

-include_lib("eunit/include/eunit.hrl").


exometer_disk_log_test() ->
    exometer:start(),

    %% A rrets reporter
    RRetsArgs = [
        {name, test_log},
        {file, "test_log"},
        {size, {1024, 4}} ],

    ok = exometer_report:add_reporter(reporter1, [
            {module, exometer_report_rrets},
            {rrets_args, RRetsArgs}]),
    [{reporter1, _Pid}] = exometer_report:list_reporters(),

    %% 
    exometer:new([g], counter),

    ok = exometer_report:subscribe(reporter1, [g], value, 100),

    exometer:update([g], 13),

    receive 
        nothing -> ok
    after 500 ->
            ok
    end,

    lager:info("Info: ~p", [exometer_report:call_reporter(reporter1, info)]),

    exometer:stop(),

    ok.

    
