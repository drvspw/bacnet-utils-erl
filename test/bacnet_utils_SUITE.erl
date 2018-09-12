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
	 write_property_request_test/1, 

	 read_octetstring_request_test/1,
	 read_analog_output_request_test/1,
	 read_analog_value_pv_test/1, 
	 read_analog_input_oos_test/1,

	 get_apdu_from_message_test/1,
	 get_pdu_type_simple_ack_test/1,
	 get_pdu_type_complex_ack_test/1,
	 get_value_from_complex_ack_test/1,

	 get_analog_value_from_complex_ack_test/1
	]).

-define(PASS(Test), ct:print("\e[1;1m ~p \e[0m\e[32m[PASS] \e[0m",[Test])).
-define(FAIL(Test), ct:print("\e[1;1m ~p \e[0m\e[31m[FAIL] \e[0m",[Test])).

-define(BACNET_TEST_GROUP, bacnet_utils_test_group).
-define(BACNET_SIMPLE_ACK_MSG, <<129,10,0,9,1,0,32,0,15>>).
-define(BACNET_COMPLEX_ACK_MSG, <<129,10,0,36,1,0,48,0,12,12,11,192,0,0,25,85,62,101,16,10,0,0,0,0,0,0,0,99,0,0,0,0,0,0,0,63>>).

all() -> [{group, ?BACNET_TEST_GROUP}].

groups() ->
    [
     {?BACNET_TEST_GROUP, 
      [parallel], 
      [read_octetstring_request_test, write_property_request_test, read_analog_output_request_test,
       read_analog_value_pv_test, read_analog_input_oos_test,
       get_apdu_from_message_test, get_pdu_type_simple_ack_test,
       get_pdu_type_complex_ack_test, get_value_from_complex_ack_test, get_analog_value_from_complex_ack_test
      ]
     }
    ].

init_per_suite(Config) ->    
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(?BACNET_TEST_GROUP, Config) ->
    [ {bacnet_simple_ack_msg, ?BACNET_SIMPLE_ACK_MSG}, {bacnet_complex_ack_msg, ?BACNET_COMPLEX_ACK_MSG} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(?BACNET_TEST_GROUP, _Config) ->
    ok.

%%===============================================
%% Test Functions
%%===============================================
write_property_request_test(_Config) ->
    Id = 1, Tag = 2,
    Wp = <<129,10,0,39,1,4,0,5,0,15,12,11,192,0,0,25,85,62,101,16,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,63,73,1>>,
    {ok, Wp} = bacnet_utils:build_write_property_request(Id, Tag),
    ?PASS('write_property_test').

read_octetstring_request_test(_Config) ->
    ObjIns = 0,
    Rp = <<129,10,0,17,1,4,0,5,0,12,12,11,192,0,0,25,85>>,
    {ok, Rp} = bacnet_utils:build_read_octetstring_pv_req(ObjIns),
    ?PASS(read_property_test).

read_analog_value_pv_test(_Config) ->
    ObjIns = 7,
    Rp = <<129,10,0,17,1,4,0,5,0,12,12,0,128,0,7,25,85>>,
    {ok, Rp} = bacnet_utils:build_read_analog_value_pv_req(ObjIns),
    ?PASS(read_analog_property_request_test).

read_analog_input_oos_test(_Config) ->
    ObjIns = 1,
    Rp = <<129,10,0,17,1,4,0,5,0,12,12,0,0,0,1,25,81>>,
    {ok, Rp} = bacnet_utils:build_read_analog_input_oos_req(ObjIns),
    ?PASS(read_analog_property_request_test).

read_analog_output_request_test(_Config) ->
    %%TODO: <<129,10,0,17,1,4,0,5,1,12,12,0,64,0,1,25,85>> - From wireshark,
    %%TODO: but <<129,10,0,17,1,4,0,5,0,12,12,0,64,0,1,25,85>> - From api. investigae,
    ObjIns = 1,
    Rp = <<129,10,0,17,1,4,0,5,0,12,12,0,64,0,1,25,85>>,    
    {ok, Rp} = bacnet_utils:build_read_analog_output_pv_req(ObjIns),
    ?PASS(read_analog_property_request_test).

get_apdu_from_message_test(Config) ->
    SimpleMsg = ?config(bacnet_simple_ack_msg, Config),
    {ok, SimpleApdu} = bacnet_utils:get_apdu_from_message(SimpleMsg),
    3 = size(SimpleApdu),

    ComplexMsg = ?config(bacnet_complex_ack_msg, Config),
    {ok, ComplexApdu} = bacnet_utils:get_apdu_from_message(ComplexMsg),
    30 = size(ComplexApdu),
    ?PASS(get_apdu_from_message_test).

get_pdu_type_simple_ack_test(Config) ->
    SimpleAckMsg = ?config(bacnet_simple_ack_msg, Config),
    {ok, SimpleApdu} = bacnet_utils:get_apdu_from_message(SimpleAckMsg),
    pdu_type_simple_ack= bacnet_utils:get_pdu_type(SimpleApdu).

get_pdu_type_complex_ack_test(Config) ->
    ComplexAckMsg = ?config(bacnet_complex_ack_msg, Config),
    {ok, ComplexApdu} = bacnet_utils:get_apdu_from_message(ComplexAckMsg),
    pdu_type_complex_ack= bacnet_utils:get_pdu_type(ComplexApdu).

get_value_from_complex_ack_test(Config) ->
    ComplexAckMsg = ?config(bacnet_complex_ack_msg, Config),
    {ok, ComplexApdu} = bacnet_utils:get_apdu_from_message(ComplexAckMsg),
    {ok, 10, 99} = bacnet_utils:get_value_from_complex_ack(ComplexApdu).

get_analog_value_from_complex_ack_test(_Config) ->
    %% Analog property value is 0.000000
    {ok, 0.0} = bacnet_utils:get_float_from_complex_ack(<<129,10,0,23,1,0,48,1,12,12,0,64,0,1,25,85,62,68,0,0,0,0,63>>),
    
    %% Analog Property values is 100.000000
    {ok, 100.00} = bacnet_utils:get_float_from_complex_ack(<<129,10,0,23,1,0,48,1,12,12,0,64,0,1,25,85,62,68,66,200,0,0,63>>),
    
    %% Analog Property value is 34.000000
    {ok, 34.0} = bacnet_utils:get_float_from_complex_ack(<<129,10,0,23,1,0,48,1,12,12,0,64,0,1,25,85,62,68,66,8,0,0,63>>).


    
