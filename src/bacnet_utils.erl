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
	 build_write_property_request/2,
	 build_read_property_request/0
]).

-on_load(init/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).

%%====================================================================
%% API functions - NIFS
%%====================================================================

%% @doc build_write_property_request/2 builds a bacnet write property request
%%
%% Generates a write property request where the property value is a concatenation
%% of  `Id' and `Tag'.
%% @end
-spec build_write_property_request(Id, Tag) -> WritePropertyRequest
    when
      Id :: iodata(),
      Tag :: binary(),
      WritePropertyRequest :: binary().
build_write_property_request(Id, Tag) when is_binary(Id), is_binary(Tag) ->
    %% TODO: Both Id and tag has to be 8 byte binaries
    build_write_property_request_nif(Id, Tag).

%% @doc build_read_property_request/0 builds a bacnet read property request
%%
%% Generates a read property request where the property value read is a 
%% concatenation of  `Id' and `Tag'.
%% @end
-spec build_read_property_request() -> ReadPropertyRequest
    when
      ReadPropertyRequest :: binary().
build_read_property_request() ->
    %% TODO: Both Id and tag has to be 8 byte binaries
    build_read_property_request_nif().
    


%%====================================================================
%% NIFS
%%====================================================================
build_write_property_request_nif(_,_) ->
    not_loaded(?LINE).

build_read_property_request_nif() ->
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
