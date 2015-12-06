%% commands
-record(make_new_draft, {
  id
}).

-record(refine_title_of_draft,{
  id,
  title
}).

%% Events
-record(new_draft_made, {
  id,
  date_created
}).

-record(title_of_draft_refined,{
  id,
  title
}).