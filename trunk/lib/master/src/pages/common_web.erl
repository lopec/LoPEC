-module(common_web).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

%% is_logged_in(Do_check) -> String
%% If do_check is true then it checks if the user is
%% logged in and if not it redirects the user to the login page.
is_logged_in(Do_check) ->
    case Do_check of 
        true ->
           case wf:user() of
                undefined ->
                    wf:redirect_to_login("/web/login");
                Username ->
                    ["Logged in as " ++ Username ++ " ",
                    #link {text="[LOGOUT]", postback=logout}]
            end;
        false ->
            "You are not logged in."
    end.

main() ->
	#template { file="./wwwroot/template.html"}.

title() ->
    "LoPEC".

footer() ->
    "LoPEC 2009".

get_info() ->
    "Low Power Erlang-based Cluster".

get_license() ->
    "The hallmark of proprietary software licenses is that the software publisher grants a license to use one or more copies of software, but that ownership of those copies remains with the software publisher (hence use of the term proprietary). One consequence of this feature of proprietary software licenses is that virtually all rights regarding the software are reserved by the software publisher. Only a very limited set of well-defined rights are conceded to the end-user. Therefore, it is typical of proprietary software license agreements to include many terms which specifically prohibit certain uses of the software, often including uses which would otherwise be allowed under copyright law.".


% Creates the menu.
menu() ->
    Event = #event { target=submenu, type=click },
    Menu = [
        #panel{id=menuitem, body=[
            #link{text="Dashboard", actions=Event#event {actions=#appear{}}}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="Node information", actions=Event#event {actions=#hide{}}}
            ]
        }
    ],
    Menu.


% Creates the submenu.        
submenu() ->
    Submenu = [
        #panel{id=menuitem, body=[
            #link{text="Current jobs"}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="Add new job", url="/web/add/job"}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="User statistics"}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="Profile"}
            ]
        },
        #panel{id=menuitem, body=[
                #link{text="LogViewer", url="/web/log/viewer"}
            ]
        }
    ],
    Submenu.