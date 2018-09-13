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
-module(bacnet_utils).

%% API exports
-export([
	 build_write_msv_req/2,

	 build_write_octetstring_req/3,


	 build_read_octetstring_pv_req/1,
	 build_read_analog_value_pv_req/1,
	 build_read_analog_input_oos_req/1,
	 build_read_analog_input_pv_req/1,
	 build_read_analog_output_pv_req/1,
	 build_read_msv_pv_req/1,

	 get_apdu_from_message/1,
	 get_pdu_type/1,

	 get_value_from_complex_ack/1,
	 get_uint_from_complex_ack/1,
	 get_float_from_complex_ack/1
]).

-on_load(init/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).

%%====================================================================
%% API functions - NIFS
%%====================================================================

%% @doc build_write_msv_req/2 builds a bacnet write property request
%%
%% Generates a write property request where the property value is a concatenation
%% of  `Id' and `Tag'.
%% @end
-spec build_write_msv_req(Instance, Value) -> WritePropertyRequest
    when
      Instance :: integer(),
      Value :: integer(),
      WritePropertyRequest :: binary().
build_write_msv_req(Id, Tag) ->
    build_write_msv_req_nif(Id, Tag).

%% @doc build_write_octetstring_req/3 builds a bacnet write property request
%%
%% Generates a write property request where the property value is a concatenation
%% of  `Id' and `Tag'. `Instance' is the bacnet object instance
%% @end
-spec build_write_octetstring_req(Instance, Id, Tag) -> WritePropertyRequest
    when
      Instance :: integer(),
      Id :: integer(),
      Tag :: integer(),
      WritePropertyRequest :: binary().
build_write_octetstring_req(Instance, Id, Tag) ->
    build_write_octetstring_req_nif(Instance, Id, Tag).

%% @doc build_read_octetstring_pv_req/1 builds a bacnet read property request
%%
%% Generates a read property request where the property that is read in `Ins'
%% @end
build_read_octetstring_pv_req(Instance) ->
    build_read_octetstring_pv_req_nif(Instance).

build_read_analog_value_pv_req(Ins) ->
    build_read_analog_value_pv_req_nif(Ins).

build_read_analog_input_oos_req(Ins) ->
    build_read_analog_input_oos_req_nif(Ins).

build_read_analog_input_pv_req(Ins) ->
    build_read_analog_input_pv_req_nif(Ins).

build_read_analog_output_pv_req(Ins) ->
    build_read_analog_output_pv_req_nif(Ins).

build_read_msv_pv_req(Ins) ->
    build_read_msv_pv_req_nif(Ins).

get_apdu_from_message(Msg) ->
    get_apdu_from_message_nif(Msg).

get_pdu_type(Apdu) ->
    get_pdu_type_nif(Apdu).

get_value_from_complex_ack(Apdu) ->
    get_value_from_complex_ack_nif(Apdu).

get_uint_from_complex_ack(Apdu) ->
    get_uint_from_complex_ack_nif(Apdu).

get_float_from_complex_ack(Apdu) ->
    get_float_from_complex_ack_nif(Apdu).

%%====================================================================
%% NIFS
%%====================================================================
build_write_msv_req_nif(_,_) ->
    not_loaded(?LINE).

build_write_octetstring_req_nif(_,_,_) ->
    not_loaded(?LINE).

get_apdu_from_message_nif(_) ->
    not_loaded(?LINE).

get_pdu_type_nif(_) ->
    not_loaded(?LINE).

get_value_from_complex_ack_nif(_) ->
    not_loaded(?LINE).

get_uint_from_complex_ack_nif(_) ->
    not_loaded(?LINE).

get_float_from_complex_ack_nif(_) ->
    not_loaded(?LINE).

build_read_octetstring_pv_req_nif(_) ->
    not_loaded(?LINE).

build_read_analog_value_pv_req_nif(_) ->
    not_loaded(?LINE).

build_read_analog_input_oos_req_nif(_) ->
    not_loaded(?LINE).

build_read_analog_input_pv_req_nif(_) ->
    not_loaded(?LINE).

build_read_analog_output_pv_req_nif(_) ->
    not_loaded(?LINE).

build_read_msv_pv_req_nif(_) ->
    not_loaded(?LINE).

%%====================================================================
%% Internal functions
%%====================================================================
init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

apdu_float(Apdu) ->
    Size = byte_size(Apdu) - 6,
    <<_Ignore:Size/binary, _Header:8, V:32/float, _:8>> = Apdu,
    {ok, V}.
