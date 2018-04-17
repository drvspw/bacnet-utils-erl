%%-------------------------------------------------------------------------------------------
%% 
%% XAPTUM CONFIDENTIAL
%% __________________
%% 
%%  2017(C) Xaptum, Inc.
%%  All Rights Reserved.Patents Pending.
%% 
%% NOTICE:  All information contained herein is, and remains
%% the property of Xaptum, Inc.  The intellectual and technical concepts contained
%% herein are proprietary to Xaptum, Inc and may be covered by U.S. and Foreign Patents,
%% patents in process, and are protected by trade secret or copyright law.
%% Dissemination of this information or reproduction of this material
%% is strictly forbidden unless prior written permission is obtained
%% from Xaptum, Inc.
%%
%% @author Venkatakumar Srinivasan
%%
%%-------------------------------------------------------------------------------------------
-module(bacnet_utils_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
	 init_per_group/2,
	 end_per_group/2,

	 init_per_suite/1,
	 end_per_suite/1,

	 all/0,
	 groups/0
]).


-export([
	 write_property_test/1, 
	 read_property_test/1
	]).

-define(PASS(Test), ct:print("\e[1;1m ~p \e[0m\e[32m[PASS] \e[0m",[Test])).
-define(FAIL(Test), ct:print("\e[1;1m ~p \e[0m\e[31m[FAIL] \e[0m",[Test])).

-define(BACNET_TEST_GROUP, bacnet_utils_test_group).

all() -> [{group, ?BACNET_TEST_GROUP}].

groups() ->
    [
     {?BACNET_TEST_GROUP, [parallel], [read_property_test, write_property_test]}
    ].

init_per_suite(Config) ->    
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(?BACNET_TEST_GROUP, Config) ->
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(?BACNET_TEST_GROUP, _Config) ->
    ok.

%%===============================================
%% Test Functions
%%===============================================
write_property_test(_Config) ->
    ok = bacnet_utils:build_write_property_request(<<>>,<<>>),
    ?PASS(write_property_test).

read_property_test(_Config) ->
    ok = bacnet_utils:build_read_property_request(),
    ?PASS(write_property_test).
    
