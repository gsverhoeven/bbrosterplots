* introduce workflow with input data in input, rosters in work and rosterbook in output.
* add tests
* setup CI/CD to test on other OSes
* check why we have player_id in the input data
* create a way to add information to coach name, such as elo rating, tournament points , naf number or fumbbl team_id
* create rosterplot() now creates all rosterplots present in input data.
maybe it is better to have it create a single roster plot, and embed that in the render_rosterbook function?
Same as with the skill tables?
* Remove dependency on webshot for the skill tables.
* Add another output format, a shiny dashboard
