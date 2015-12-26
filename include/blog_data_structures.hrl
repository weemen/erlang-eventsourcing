%% commands
-record(make_new_draft, {
  id
}).

-record(refine_title_of_draft,{
  id,
  title
}).

-record(refine_content_of_draft,{
  id,
  content
}).

-record(publish_draft,{
  id
}).

-record(unpublish_draft,{
  id
}).

-record(renew_draft,{
  id
}).

-record(hide_draft,{
  id
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

-record(content_of_draft_refined,{
  id,
  content
}).

-record(draft_published,{
  id
}).

-record(draft_unpublished,{
  id
}).

-record(draft_renewed,{
  id,
  followUpId
}).

-record(draft_hidden,{
  id
}).