My First Erlang Application
=======
This is my first experience with Erlang.
I've tried to build an event-sourced application with mysql as eventstore.

What did I do:
=======
I took a project from Brian Hunter (https://github.com/bryanhunter/cqrs-with-erlang/tree/ndc-oslo) which was a part of this talk https://vimeo.com/97318824.
This was a good base to make changes on:
 
- Swapped ETS to MySQL as eventstore to make my events persistent. (This was actually a lot work)


- Stored all events as JSON in MySQL.
This is actually an ongoing expiriment. I found out that the "Record" data type didn't work well for me when retrieving records
from the eventstore (My n00b erlang skills can be the problem as well!). The other reason is that I may want to create different read applications which
will deal with incoming events. To publish those events and make them directly usable for example
a php application it sounded like a good idea to store everything in JSON. I'm not convinced yet so this might be change in the future.


- Created a simple projection


- Swapped context. It's not about bankaccounts anymore. It's a about a blog and its drafts. 

Feedback
========
What I need now is feedback, feedback to upgrade my erlang skills. 
One of the things I noticed was that if I want to add a new event I have to change code on many places. Is this normal?
Maybe someone can point me to great resources where I can learn from.

Getting started
=======

```
git clone https://github.com/weemen/erlang-eventsourcing.git
rebar get-deps
rebar compile
rebar shell
application:start(emysql).
emysql:add_pool(erlang_es_blog, [{size,1},
                     {user,"root"},
                     {password,"root"},
                     {database,"erlang_es"},
                     {encoding,utf8}]).
application:start(blog).
```


```
blog:make_new_draft().
blog:refine_title_of_draft("21d84c10-30b0-4bf6-9c3d-b308c8c15afc","some title 27").
blog:refine_content_of_draft("21d84c10-30b0-4bf6-9c3d-b308c8c15afc","this is my content").
blog:publish_draft("21d84c10-30b0-4bf6-9c3d-b308c8c15afc").
blog:hide_draft("21d84c10-30b0-4bf6-9c3d-b308c8c15afc").
blog:renew_draft("21d84c10-30b0-4bf6-9c3d-b308c8c15afc").
```

Things still todo
=======
- Create more projections
- Separate the projections from this application (by publishing events over something like rabbitmq)
- Write tests
- Simplefy code and remove the odd stuff.
- There is no real playhead functionality so events might still collide with each other.
- Mysql should be started out of the box
- Create decent log message instead of io:fwrite.