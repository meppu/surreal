-module(example_users).

-export([add/2, get/1, get_all/0, verify/1, remove/1]).

add(Name, Age) ->
  Data = #{<<"age">> => Age, <<"verified">> => false},
  surreal:create(db_conn, {"users", Name}, Data).

get(Name) ->
  surreal:select(db_conn, {"users", Name}).

get_all() ->
  surreal:select(db_conn, "users").

verify(Name) ->
  surreal:merge(db_conn, {"users", Name}, #{<<"verified">> => true}).

remove(Name) ->
  surreal:delete(db_conn, {"users", Name}).