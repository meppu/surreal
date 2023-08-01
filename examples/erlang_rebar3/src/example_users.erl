-module(example_users).

-export([add/2, get/1, get_all/0, verify/1, remove/1]).

add(Name, Age) ->
  Data = #{<<"age">> => Age, <<"verified">> => false},
  surreal:create(db_conn, create_id(Name), Data).

get(Name) ->
  surreal:select(db_conn, create_id(Name)).

get_all() ->
  surreal:select(db_conn, "users").

verify(Name) ->
  surreal:merge(db_conn, create_id(Name), #{<<"verified">> => true}).

remove(Name) ->
  surreal:delete(db_conn, create_id(Name)).


%% @private
create_id(Name) ->
  string:concat("users:", Name).