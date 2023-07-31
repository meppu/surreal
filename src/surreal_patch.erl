%%%-------------------------------------------------------------------------
%%% @copyright (C) 2023, meppu
%%% @doc Erlang-y JSON patch.
%%%
%%% Note that this module doesn't implement JSON patch itself but types and converter to make {@link surreal:patch/3} easier.
%%%
%%% You can learn more about JSON Patch <a href="https://jsonpatch.com/" target="_blank">here</a>.
%%%
%%% @author meppu
%%% @end
%%%-------------------------------------------------------------------------
-module(surreal_patch).

-export([convert/1]).
-export_type([patch/0]).

%%%==========================================================================
%%%
%%%   Public types
%%%
%%%==========================================================================

-type patch() ::
    patch_add()
    | patch_remove()
    | patch_replace()
    | patch_copy()
    | patch_move()
    | patch_test().
%% Combination of all patch types.

-type patch_add() :: {add, Path :: iodata(), Value :: term()}.
%% Adds a value to an object or inserts it into an array.
%% In the case of an array, the value is inserted before the given index.
%% The `-' character can be used instead of an index to insert at the end of an array.

-type patch_remove() :: {remove, Path :: iodata()}.
%% Removes a value from an object or array.

-type patch_replace() :: {replace, Path :: iodata(), Value :: term()}.
%% Replaces a value. Equivalent to a "remove" followed by an "add".

-type patch_copy() :: {copy, From :: iodata(), Path :: iodata()}.
%% Copies a value from one location to another within the JSON document.
%% Both `From' and `Path' are JSON Pointers.

-type patch_move() :: {move, From :: iodata(), Path :: iodata()}.
%% Moves a value from one location to the other.
%% Both `From' and `Path' are JSON Pointers.

-type patch_test() :: {test, Path :: iodata(), Value :: term()}.
%% Tests that the specified value is set in the document.
%% If the test fails, then the patch as a whole should not apply.

%%%==========================================================================
%%%
%%%   Public functions
%%%
%%%==========================================================================

%%-------------------------------------------------------------------------
%% @doc Convert given tuple / list into JSON patch map / list.
%% @end
%%-------------------------------------------------------------------------
-spec convert(Patch :: patch() | list(patch())) -> map() | list(map()).
convert(Patches) when is_list(Patches) ->
    [convert(I) || I <- Patches];
convert({add, Path, Value}) ->
    #{
        <<"op">> => <<"add">>,
        <<"path">> => unicode:characters_to_binary(Path),
        <<"value">> => Value
    };
convert({remove, Path}) ->
    #{
        <<"op">> => <<"remove">>,
        <<"path">> => unicode:characters_to_binary(Path)
    };
convert({replace, Path, Value}) ->
    #{
        <<"op">> => <<"replace">>,
        <<"path">> => unicode:characters_to_binary(Path),
        <<"value">> => Value
    };
convert({copy, From, Path}) ->
    #{
        <<"op">> => <<"copy">>,
        <<"from">> => unicode:characters_to_binary(From),
        <<"path">> => unicode:characters_to_binary(Path)
    };
convert({move, From, Path}) ->
    #{
        <<"op">> => <<"move">>,
        <<"from">> => unicode:characters_to_binary(From),
        <<"path">> => unicode:characters_to_binary(Path)
    };
convert({test, Path, Value}) ->
    #{
        <<"op">> => <<"test">>,
        <<"path">> => unicode:characters_to_binary(Path),
        <<"value">> => Value
    }.
