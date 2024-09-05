# Tournament Simulation in Erlang

## Overview

This project simulates a tournament where players compete against each other in games. The simulation handles player management, game scheduling, and keeps track of player credits and game outcomes. The project consists of Erlang source files and a sample player data file.

## Files

- `game.erl`: Contains the master process logic that manages the tournament. This process schedules games, handles invitations, and determines game outcomes.
- `player.erl`: Defines the player processes. Each player handles game requests and manages their credits.

## Getting Started

### Prerequisites

- Erlang installed on your system. You can download and install it from [Erlang's official website](https://www.erlang.org/downloads).

### Compilation

Compile the Erlang files using the following commands:

```bash
erlc game.erl
erlc player.erl
