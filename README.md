Rock/Paper/Scissors Tournament Simulation in Erlang
Overview
This project simulates a Rock/Paper/Scissors (RPS) tournament using Erlang’s concurrent programming capabilities. The simulation involves multiple players engaging in games with each other, managed by a master process. The system models player interactions, game scheduling, outcome determination, and player disqualifications, providing real-time logging and a final tournament report.

Features
Multi-Process Architecture: Utilizes Erlang’s concurrency model to create separate processes for each player and a master process.
Dynamic Game Management: Players interact by sending game requests and receiving invitations, with the master process handling game scheduling and outcome determination.
Comprehensive Game Logic: Implements RPS rules, including handling ties, determining winners, and managing player credits and disqualifications.
Real-Time Logging: Provides detailed logs of game activities and player statuses, including new game scheduling, game results, and disqualification events.
Tournament Reporting: Generates a final report summarizing the tournament, including player performance and the winner.
Getting Started
Prerequisites
Erlang/OTP 23 or higher. Ensure Erlang is installed and accessible from your command line.
Installation
Clone the Repository

bash
Copy code
git clone https://github.com/yourusername/rps-tournament-erlang.git
cd rps-tournament-erlang
Compile the Erlang Code

Compile the Erlang source files using the erlc command:

bash
Copy code
erlc game.erl player.erl
Running the Simulation
To start the tournament simulation, use the following command:

bash
Copy code
erl -noshell -run game start player_file.txt -s init stop
Replace player_file.txt with the path to your player data file. The player data file should contain Erlang tuples in the format {name, credits}, where name is the player's name (an atom) and credits is the number of game credits.

Example
If your player file (players.txt) contains:

erlang
Copy code
{sam, 26}.
{jill, 12}.
{ahmad, 17}.
Run the simulation with:

bash
Copy code
erl -noshell -run game start players.txt -s init stop
Code Structure
game.erl: Contains the master process logic. This module manages the overall tournament, schedules games, tracks player credits, and generates the final report.

player.erl: Contains the player process logic. Each player process handles game invitations, responses, and makes random RPS moves.

Game Flow
Initialization: The game module reads the player data file and spawns a process for each player.
Game Requests: Players randomly select other players to challenge and send game requests.
Game Scheduling: The master process schedules new games, assigns unique IDs, and handles game outcomes.
Game Outcomes: Players submit their moves. The master process determines the winner, handles ties, and updates player credits.
Disqualification: Players with zero credits are disqualified and stopped from making new game requests.
Tournament End: Once only one player remains, the master process announces the winner and generates a tournament summary.
