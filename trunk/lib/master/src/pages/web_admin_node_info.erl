-module(web_admin_node_info).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).


main() ->
    %% Only admins are supposed to see this page
    case common_web:have_role([role_admin]) of
        false ->
            wf:redirect("/web/index");
        _ -> []
    end,
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer().

get_info() ->
    common_web:get_info().

menu() ->
    common_web:menu().

submenu() ->
    common_web:submenu().

user_status() ->
    common_web:is_logged_in(true).


get_nodeslist([]) ->
    [];
get_nodeslist([H|T]) ->
    [
        #span{text=lists:concat([H])},
        #br{}|
        get_nodeslist(T)].


body() ->
    Body = [
       #h2{text="Nodes connected to the cluster"},
       get_nodeslist(nodes())
    ],
    wf:render(Body).

event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/index");
event(_) -> ok.
