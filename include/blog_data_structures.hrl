%% commands
-record(make_new_draft, {
  id
}).

%% Events
-record(new_draft_made, {
  id,
  date_created
}).