# Rock/Paper/Scissors Tournament Simulation

Welcome to the Rock/Paper/Scissors Tournament simulation project! This project is designed to simulate a gaming tournament where players interact through the classic Rock/Paper/Scissors game using Erlang's powerful concurrency model. The game consists of players with a certain number of credits, and they compete until only one player remains.

## Features

- **Multi-Process Simulation:** Utilizes Erlang’s concurrency model to create individual processes for each player, simulating a real-time tournament environment.
- **Dynamic Game Scheduling:** Players make requests and respond to invitations asynchronously, with a master process managing game scheduling and outcome determination.
- **Credit Management:** Players have a set number of credits, which are decremented based on game outcomes. Players are disqualified when they run out of credits.
- **Real-Time Logging:** The master process outputs detailed logs of game activities, including new game scheduling, game results, and player disqualification.

## Getting Started

To get started with the Rock/Paper/Scissors Tournament simulation:

1. **Clone the Repository**

   ```bash
   git clone https://github.com/yourusername/rock-paper-scissors-tournament.git
   cd rock-paper-scissors-tournament
   
2. Compile the Erlang files using the following commands:

   ```bash
   erlc game.erl
   erlc player.erl

3. Start the simulation using the following command, specifying the player data file (e.g., players.txt):

   ```bash
   erl -noshell -run game start players.txt -s init stop

## File Descriptions
- **game.erl**   Contains the master process logic that manages the tournament. This process schedules games, handles invitations, and determines game outcomes.
- **player.erl**   Defines the player processes. Each player handles game requests and manages their credits.


