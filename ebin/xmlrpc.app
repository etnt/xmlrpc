                                    %% This is the application resource file (.app file) for the 'base'
%% application.
{application, xmlrpc,
[{description, "An HTTP 1.1 compliant XML-RPC library for Erlang"},
 {vsn, "0.1.0"},
 {modules, [example_serv,
	tcp_serv,
	test,
	xmlrpc_decode,
	xmlrpc_encode,
	xmlrpc_http,
	xmlrpc_util,
	xmlrpc]},
 {registered,[xmlrpc]},
 {applications, [kernel,stdlib]},
 {mod, {xmlrpc,[]}},
 {start_phases, []}
]}.


