%%%-------------------------------------------------------------------------
%%% @copyright (C) 2023, meppu
%%% @since 2.1.0
%%% @doc Module for representing Table or Record ID.
%%%
%%% @author meppu
%%% @end
%%%-------------------------------------------------------------------------
-module(surreal_entity).

-export_type([thing/0]).
-export([convert/1]).

%%%==========================================================================
%%%
%%%   Public types
%%%
%%%==========================================================================

-type thing() :: entity_identifier() | record_specification().
%% A type that can represent either an entity identifier or a record specification.

-type entity_identifier() :: iodata().
%% An identifier that serves as a unique reference for entities within a database,
%% encompassing both database tables and record identifiers.
%%
%% - `"employees"': Represents the table containing employee records.
%%
%% - `"employees:meppu"': Identifies a specific employee record.

-type record_specification() :: {Table :: iodata(), Id :: iodata()}.
%% A tuple representing a database record's location using the table name and a unique identifier.
%%
%% - `{"employees", "meppu"}': Refers to an employee record in the `employees' table.

%%%==========================================================================
%%%
%%%   Public functions
%%%
%%%==========================================================================

%%-------------------------------------------------------------------------
%% @doc Converts given {@link thing()} to {@link entity_identifier()}.
%% @end
%%-------------------------------------------------------------------------
-spec convert(Thing :: thing()) -> entity_identifier().
convert({Table, Id}) ->
    io_lib:format("~s:~s", [Table, Id]);
convert(Other) ->
    Other.
