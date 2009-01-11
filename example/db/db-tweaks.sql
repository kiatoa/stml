>-- create table polls(id serial not null,poll_type text,title text,description text,poll_state text);
-- create table poll_categories(id serial not null,poll_id integer,description text);
-- create table poll_votes(id serial not null,period integer,poll_type text,poll_category text,voter_group integer, votes integer);

-- create table vote_items (id serial primary key,type integer,item_id integer,item_level text,town_votes integer,state_votes integer,country_votes integer,world_votes integer);
-- 
-- alter table vote_items alter column town_votes set default 0;
-- alter table vote_items alter column state_votes set default 0;
-- alter table vote_items alter column country_votes set default 0;
-- alter table vote_items alter column world_votes set default 0;
-- 
-- alter table poll_items add column class_0 int4;
-- alter table poll_items add column class_1 int4;
-- alter table poll_items add column class_2 int4;
-- 
-- alter table poll_items add column classp_0 int4;
-- alter table poll_items add column classp_1 int4;
-- alter table poll_items add column classp_2 int4;
-- 
-- alter table poll_items alter column classp_0 set default 0;
-- alter table poll_items alter column classp_1 set default 0;
-- alter table poll_items alter column classp_2 set default 0;
-- 
-- alter table poll_items add column suggestor int4;
-- 
-- alter table poll_items alter column class_0 set default 0;
-- alter table poll_items alter column class_1 set default 0;
-- alter table poll_items alter column class_2 set default 0;
-- 
-- alter table poll_items add column status int4;
-- alter table poll_items alter column status set default 0;

-- alter table poll_items add column url text;
-- alter table vote_items add column submit_date date;
-- alter table poll_items add column submit_date date;

-- alter table people add column pt_balance int4;
-- alter table people alter column pt_balance set default 0;

-- alter table people add column cert_date date;
-- alter table people alter column pt_balance set default 0;

-- create table pt_transactions (id serial not null,from_id integer,to_id integer,amount integer,transaction_time timestamp);
-- alter table pt_transactions alter column amount set default 0;

-- alter table classifieds add column points int4;
-- alter table classifieds alter column points set default 0;

-- alter table pt_transactions add column comment text;
-- alter table pt_transactions add column comment text;

-- create table temp_key(id serial not null,key  text,sent_date date);
-- alter table people add column lastlogin timestamp;

-- create table pictures(id serial not null,owner integer,size integer,name  text,type text,md5sum text,uploaded date);
-- alter table pictures add column status text;

-- create table pic_allocation(id serial not null,picnum integer,used_by integer);

-- alter table posts add column url text;
-- alter table posts add column blurb text;

insert into subjects (subjectid,subject,item_type,description) values('VoSp','Spanish','lang','Basic Spanish Vocabulary');
insert into subjects (subjectid,subject,item_type,description) values('HoMe','Homeopathy','Info','Basic Homeopathy');

alter table items add column group_name text;
alter table items add column state int4;

create table sessions (id serial not null,session_key text);
create table session_vars (id serial not null,session_id integer,page text,key text,value text);

alter table poll_items add column num_voted  integer default 0;
alter table poll_items add column vote_tot   integer default 0;
alter table poll_items add column item_votes integer default 0;

-- remember ballots are used for many things other than polls!!!!!!!!
create table ballots (id serial not null, item_id integer, class_id integer, votes integer, type_id integer);
create table ballot_classes (id serial not null, name text, pts_per_vote integer); -- join with ballots to sum up votes (pts are really votes)
insert into ballot_classes values (0,'',1);
insert into ballot_classes values (1,'',2);
insert into ballot_classes values (2,'',10);
insert into ballot_classes values (3,'',20);
insert into ballot_classes values (4,'',45);
insert into ballot_classes values (5,'',90);
insert into ballot_classes values (6,'',105);
insert into ballot_classes values (7,'',145);
insert into ballot_classes values (8,'',205);
insert into ballot_classes values (9,'',245);

create table ballot_types (id serial not null, name text);                         -- poll plurality = 0, poll approval = 1
insert into ballot_types (id,name) values (0,'poll plurality');
insert into ballot_types (id,name) values (1,'poll approval');

alter table voted add column type_id integer;
alter table voted add column id serial not null;
create table voted_types (id serial not null, name text);
insert into voted_types (id, name) values (0, 'poll vote');                -- YES!!! WE DO NEED voted_types SEPERATE FROM ballot_types
insert into voted_types (id, name) values (1, 'council vote for poll');    -- yes, they are similar but I think combining them would be
insert into voted_types (id, name) values (2, 'council vote for item');    -- painful.
insert into voted_types (id, name) values (3, 'council vote for story');

alter table people add column email_validated integer default 0;  -- has email been validated? Hmmm... should this be a seperate table
alter table people add column grade integer default 0;            -- 

alter table voted add column grade integer default 0;

-- grade
-- 
-- 0 - no status (refusing cookies)
-- 1 - has session
-- 2 - logged in, has user id
-- 3 - email validated
-- 4 ++ add 1 for every 20 points of cert_level

alter table poll_items drop column class_0  ;
alter table poll_items drop column class_1  ;
alter table poll_items drop column class_2  ;
alter table poll_items drop column classp_0 ;
alter table poll_items drop column classp_1 ;
alter table poll_items drop column classp_2 ;
alter table poll_items drop column votes    ;
alter table poll_items drop column vote_tot ;
alter table poll_items drop column num_voted;

alter table poll_items add column a_vote_tot integer default 0; -- approval  votes total
alter table poll_items add column p_vote_tot integer default 0; -- plurality votes total

alter table people alter column num set default 0;
alter table polls add column discussion_id integer default 0;

create table poll_status (id serial not null, name text);
insert into poll_status (id,name) values (0, 'In queue'); -- just posted and in queue
insert into poll_status (id,name) values (1, 'Posted');   -- published to discussion

-- fix default cert_level
alter table people alter column cert_level set default 0;
update people set cert_level=0 where cert_level is NULL;

create table discussions (id serial not null,type_id integer,activity_state integer);
update posts set thread=id where parent=0; -- was this necessary?

insert into discussions select id,0,1 from posts where parent=0;

-- ======================================================================
-- New council stuff
--======================================================================

create table councils (id serial not null, name text, discussion_id integer default 0);
alter table  council_members add column join_date date;

-- DONE ON TANG UP TO HERE

--======================================================================
-- New locations table
--======================================================================

create table locations
         (id serial not null, parent_id integer default 0, 
          council_id integer,nick text, fullname text, 
          level_id integer, blurb text, pict_id integer);
insert into locations(council_id,nick,fullname,level_id,blurb)
    values(0,'','World',0,'Our beloved Planet Earth');
insert into locations(council_id,nick,fullname,level_id,blurb)
    values(1,'us','United States',1,'The Land of the Free');
insert into locations(parent_id,council_id,nick,fullname,level_id,blurb)
    values(1,2,'az','Arizona',2,'It''s a dry heat');

drop table location;
drop table towns;
drop table states;
drop table neighborhoods ;
drop table countries;
