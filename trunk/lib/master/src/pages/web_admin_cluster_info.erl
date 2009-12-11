-module(web_admin_cluster_info).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).


main() ->
    case common_web:have_role([role_admin]) of
        true -> [];
        _ -> wf:redirect("/web/index")
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


body() ->
    Body = [
        #h2 { text="Cluster information" },
        #p{},
        #label { text=statistician:get_cluster_stats(string) }
        
    ],
    wf:render(Body).


event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");
event(_) -> ok.

