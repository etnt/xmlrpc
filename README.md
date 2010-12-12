XML-RPC client and server for Erlang
====================================

Patches
-------

 * Support HTTP 1.0 servers which do keep alive.
 * Update to newer Erlang releases from Tangentsoft applied.
   (from http://tangentsoft.net/erlang/xmlrpc-1.13-wy.patch)
 * Bug fixes from [Eric Liang][2] <eric.l.2046@gmail.com>

-- [John Wright][1] <john@dryfish.org>

----------------------------------------------------------------------------

The following is the contents of the original README.  The link to get the
latest version of this library is now defunct and I have not been able to
find an updated link.

> This is an HTTP 1.1 compliant XML-RPC library for Erlang. It is
> designed to make it easy to write XML-RPC Erlang clients and/or
> servers. The library is compliant with the XML-RPC specification
> published by http://www.xmlrpc.org/.
> 
> Prior to using this library you need a recent installation of
> Erlang. Get it at http://www.erlang.org/. You furthermore need to
> install the xmerl package, version 0.18 or better. Get it at
> http://sourceforge.net/projects/sowap/.
> 
> In order compile the library you need to update the XMERL_PATH
> variable in src/Makefile to point at your xmerl installation.
> 
> After this go to the src/ directory and run make to compile.
> 
> You are now ready to try the client and server examples in the
> examples/ directory.
> 
> Do not forget to read doc/xmlrpc.3 (or xmlrpc.txt, xmlrpc.ps,
> xmlrpc.pdf) for a detailed API description.
> 
> Get the latest version of this library at
> http://www.gleipnir.com/xmlrpc/.
> 
> Send questions and/or suggestions to jocke@gleipnir.com.

[1]: http://github.com/dryfish "John Wright"
[2]: http://github.com/ericliang "Eric Liang"
