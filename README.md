Erlang event-sourcing test with rabbitmq for queing and mysql as event-store.

```
application:start(emysql).
emysql:add_pool(erlang_es_blog, [{size,1},
                     {user,"root"},
                     {password,"root"},
                     {database,"erlang_es"},
                     {encoding,utf8}]).
application:start(blog).
```