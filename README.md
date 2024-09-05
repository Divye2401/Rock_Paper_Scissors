# Rock/Paper/Scissors Tournament Simulation

 This project simulates a tournament using the classic Rock/Paper/Scissors game, where players interact and compete until only one remains. The simulation is implemented using Erlang, leveraging its concurrency features to manage multiple players and games.

## Overview

The simulation consists of multiple players, each with a certain number of credits. The players compete in games of Rock/Paper/Scissors, and the outcomes are used to determine how many credits each player has left. Players are eliminated when their credits run out. The master process coordinates the games and logs the results.

## Features

- **Multi-Process Architecture:** Each player is represented by an individual Erlang process, allowing for concurrent game handling and interaction.
- **Dynamic Game Scheduling:** The master process schedules games dynamically and handles player invitations and responses asynchronously.
- **Credit Management:** Players start with a set number of credits and lose credits based on game outcomes. Players are disqualified when they have no credits left.
- **Detailed Logging:** The master process provides detailed logs of game activities, including new game schedules, results, and player disqualification.

## Getting Started

To get started with the Rock/Paper/Scissors Tournament simulation, follow these steps:

1. **Clone the Repository**

   ```bash
   git clone https://github.com/yourusername/rock-paper-scissors-tournament.git
   cd rock-paper-scissors-tournament
2. Compile the Erlang Files

Compile the Erlang files using erlc:
erlc game.erl
erlc player.erl

