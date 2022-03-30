import argparse
from collections import deque
from copy import deepcopy


class DieGame(object):
    """
    Die Game class

    Attributes
    ----------
    player1_position : int
        On the circular board numbered 1 to 10, denotes the number player 1 stands on
    player2_position : int
        On the circular board numbered 1 to 10, denotes the number player 2 stands on
    player1_score : int
        Tells player 1's score so far.
    player2_score : int
        Tells player 2's score so far.
    die_roll_count : int
        Counter for number of times the die has been rolled so far
    turn : int
        Denotes the player whose turn it is to roll the die
    """
    def __init__(self, player1_start_position, player2_start_position):
        """
        Class initializer

        Parameters
        ----------
        player1_start_position : int
            Determines the start position of player 1
        player2_start_position : int
            Determines the start position of player 2

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> assert object1.player1_position == 4
        >>> assert object1.player2_position == 8
        >>> assert object1.player1_score == 0
        >>> assert object1.player2_score == 0
        >>> assert object1.die_roll_count == 0
        >>> assert object1.turn == 1
        """
        self.player1_position = player1_start_position
        self.player2_position = player2_start_position
        self.player1_score = 0
        self.player2_score = 0
        self.turn = 1
        self.die_roll_count = 0

    def __str__(self):
        """
        String representation of the class

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> print(super(DeterministicDie, object1).__str__())
        Player 1 position: 4
        Player 2 position: 8
        Player 1 score   : 0
        Player 2 score   : 0
        Die rolls        : 0
        Turn             : 1
        """
        returning_str = "Player 1 position".ljust(17) + ": " + str(self.player1_position) + '\n'
        returning_str += "Player 2 position".ljust(17) + ": " + str(self.player2_position) + '\n'
        returning_str += "Player 1 score".ljust(17) + ": " + str(self.player1_score) + '\n'
        returning_str += "Player 2 score".ljust(17) + ": " + str(self.player2_score) + '\n'
        returning_str += "Die rolls".ljust(17) + ": " + str(self.die_roll_count) + '\n'
        returning_str += "Turn".ljust(17) + ": " + str(self.turn)

        return returning_str

    def play_next_turn(self, die_roll_sum):
        """
        The next turn is played adjusting the game state accordingly

        Parameters
        ----------
        die_roll_sum : int
            Sum of 3 die rolls

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> super(DeterministicDie, object1).play_next_turn(3)
        >>> object1.player1_position
        7
        >>> object1.player2_position
        8
        >>> object1.player1_score
        7
        >>> object1.player2_score
        0
        >>> object1.die_roll_count
        3
        >>> object1.turn
        2
        >>> super(DeterministicDie, object1).play_next_turn(3)
        >>> object1.player1_position
        7
        >>> object1.player2_position
        1
        >>> object1.player1_score
        7
        >>> object1.player2_score
        1
        >>> object1.die_roll_count
        6
        >>> object1.turn
        1
        """
        if self.turn == 1:
            self.player1_position = self.player1_position + die_roll_sum \
                                if (self.player1_position + die_roll_sum) % 10 == 0 \
                              else (self.player1_position + die_roll_sum) % 10
            self.player1_score = self.player1_score + self.player1_position
            self.turn = 2
        elif self.turn == 2:
            self.player2_position = self.player2_position + die_roll_sum \
                                if (self.player2_position + die_roll_sum) % 10 == 0 \
                              else (self.player2_position + die_roll_sum) % 10
            self.player2_score = self.player2_score + self.player2_position
            self.turn = 1
        self.die_roll_count += 3


class DiracDie():
    """
    Die game with Dirac Die

    Attributes
    ----------
    player1_wins : int
        Number of universes player 1 has won in
    player2_wins : int
        Number of universes player 2 has won in
    game_results : dict
        Dictionary ((player1_position, player2_position, player1_score, player2_score, player_turn, roll) ->
        #player1 wins, #player2 wins) of game state and the corresponding result.

    """
    def __init__(self, player1_start_position, player2_start_position, target_score):
        """
        Class initializer

        Parameters
        ----------
        player1_start_position : int
            Determines the start position of player 1
        player2_start_position : int
            Determines the start position of player 2

        Example:
        >>> object1 = load_data("test.txt", 2)
        >>> assert object1.player1_start_position == 4
        >>> assert object1.player2_start_position == 8
        >>> assert object1.player1_wins == 0
        >>> assert object1.player2_wins == 0
        >>> assert len(object1.game_results) == 0
        """
        self.player1_start_position = player1_start_position
        self.player2_start_position = player2_start_position
        self.player1_wins = 0
        self.player2_wins = 0
        self.game_results = {}
        self.target_score = target_score

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1 = load_data("test.txt", 2)
        >>> print(object1)
        Player 1 start   : 4
        Player 2 start   : 8
        Player 1 wins    : 0
        Player 2 wins    : 0
        Game results     : 0
        Target score     : 21
        """
        returning_str = "Player 1 start".ljust(17) + ": " + str(self.player1_start_position) + '\n'
        returning_str += "Player 2 start".ljust(17) + ": " + str(self.player2_start_position) + '\n'
        returning_str += "Player 1 wins".ljust(17) + ": " + str(self.player1_wins) + '\n'
        returning_str += "Player 2 wins".ljust(17) + ": " + str(self.player2_wins) + '\n'
        returning_str += "Game results".ljust(17) + ": " + str(len(self.game_results)) + '\n'
        returning_str += "Target score".ljust(17) + ": " + str(self.target_score)

        return returning_str

    def play_game(self, game, die_roll_sum):
        """
        Play a single game

        3: 1 - (1, 1, 1)
        4: 3 - (1, 1, 2), (1, 2, 1), (2, 1, 1)
        5: 6 - (1, 1, 3), (1, 3, 1), (3, 1, 1), (1, 2, 2), (2, 1, 2), (2, 2, 1)
        6: 7 - (1, 2, 3), (1, 3, 2), (3, 1, 2), (2, 1, 3), (2, 3, 1), (3, 2, 1), (2, 2, 2)
        7: 6 - (1, 3, 3), (3, 1, 3), (3, 3, 1), (2, 2, 3), (2, 3, 2), (3, 2, 2)
        8: 3 - (2, 3, 3), (3, 2, 3), (3, 3, 2)
        9: 1 - (3, 3, 3)

        Parameters
        ----------
        game : DieGame
            Die game state
        die_roll_sum : int
            The die roll

        Returns
        -------
        (#player_1_wins, #player_2_wins)
            Number of wins for each player

        Example:
        """
        player1_position = game.player1_position
        player2_position = game.player2_position
        player1_score = game.player1_score
        player2_score = game.player2_score
        player_turn = game.turn

        # Check whether game_results contains the result
        if (player1_position, player2_position, player1_score, player2_score, player_turn, die_roll_sum) in self.game_results:
            return self.game_results[(player1_position, player2_position, player1_score, player2_score, player_turn,
                                      die_roll_sum)]

        # Play the next turn
        game.play_next_turn(die_roll_sum)

        # If game ended, update the appropriate player's #wins and add entry to game_results
        if game.player1_score >= self.target_score:
            self.game_results[(player1_position, player2_position, player1_score, player2_score, player_turn,
                               die_roll_sum)] = (1, 0)
            return 1, 0
        elif game.player2_score >= self.target_score:
            self.game_results[(player1_position, player2_position, player1_score, player2_score, player_turn,
                               die_roll_sum)] = (0, 1)
            return 0, 1

        # Calculate the total wins by each player for each triple die roll that was played.
        total_player1_wins = 0
        total_player2_wins = 0
        for three_die_roll_sum in range(3, 10):
            player_1_wins, player_2_wins = self.play_game(deepcopy(game), three_die_roll_sum)

            if three_die_roll_sum in [3, 9]:
                total_player1_wins += player_1_wins
                total_player2_wins += player_2_wins
            elif three_die_roll_sum in [4, 8]:
                total_player1_wins += 3 * player_1_wins
                total_player2_wins += 3 * player_2_wins
            elif three_die_roll_sum in [5, 7]:
                total_player1_wins += 6 * player_1_wins
                total_player2_wins += 6 * player_2_wins
            elif three_die_roll_sum == 6:
                total_player1_wins += 7 * player_1_wins
                total_player2_wins += 7 * player_2_wins

        self.game_results[(player1_position, player2_position, player1_score, player2_score, player_turn,
                           die_roll_sum)] = (total_player1_wins, total_player2_wins)
        return total_player1_wins, total_player2_wins

    def play_game_driver(self):
        """
        Play dirac die game.

        3: 1 - (1, 1, 1)
        4: 3 - (1, 1, 2), (1, 2, 1), (2, 1, 1)
        5: 6 - (1, 1, 3), (1, 3, 1), (3, 1, 1), (1, 2, 2), (2, 1, 2), (2, 2, 1)
        6: 7 - (1, 2, 3), (1, 3, 2), (3, 1, 2), (2, 1, 3), (2, 3, 1), (3, 2, 1), (2, 2, 2)
        7: 6 - (1, 3, 3), (3, 1, 3), (3, 3, 1), (2, 2, 3), (2, 3, 2), (3, 2, 2)
        8: 3 - (2, 3, 3), (3, 2, 3), (3, 3, 2)
        9: 1 - (3, 3, 3)

        Example:
        >>> object1 = load_data("test.txt", 2)
        >>> object1.target_score = 1
        >>> object1.play_game_driver()
        >>> assert object1.player1_wins == 27
        >>> assert object1.player2_wins == 0

        >>> object2 = load_data("test.txt", 2)
        >>> object2.target_score = 2
        >>> object2.play_game_driver()
        >>> assert object2.player1_wins == 183
        >>> assert object2.player2_wins == 156

        >>> object3 = load_data("test.txt", 2)
        >>> object3.play_game_driver()
        >>> assert object3.player1_wins == 444356092776315
        >>> assert object3.player2_wins == 341960390180808
        """
        # Initialize the game state to the beginning
        game = DieGame(self.player1_start_position, self.player2_start_position)

        for three_die_roll_sum in range(3, 10):
            player_1_wins, player_2_wins = self.play_game(deepcopy(game), three_die_roll_sum)

            if three_die_roll_sum in [3, 9]:
                self.player1_wins += player_1_wins
                self.player2_wins += player_2_wins
            elif three_die_roll_sum in [4, 8]:
                self.player1_wins += 3 * player_1_wins
                self.player2_wins += 3 * player_2_wins
            elif three_die_roll_sum in [5, 7]:
                self.player1_wins += 6 * player_1_wins
                self.player2_wins += 6 * player_2_wins
            elif three_die_roll_sum == 6:
                self.player1_wins += 7 * player_1_wins
                self.player2_wins += 7 * player_2_wins


class DeterministicDie(DieGame):
    """
    Die game with Deterministic Die

    Attributes
    ----------
    three_die_roll_sum_mod_10_dict : dict
        (Units place of first die roll -> Units place of sum of three die rolls)
        We only need to consider the units place of the summation of the three numbers on the die to move pawn.
        Player 1's die roll units places and the units places of their sum
        1,2,3=6, 7,8,9=4, 3,4,5=2, 9,0,1=0, 5,6,7=8
        Player 2's die roll units places and the units places of their sum
        4,5,6=5, 0,1,2=3, 6,7,8=1, 2,3,4=9, 8,9,0=7
    """

    def __init__(self, player1_start_position, player2_start_position):
        """
        Class initializer

        Parameters
        ----------
        player1_start_position : int
            Determines the start position of player 1
        player2_start_position : int
            Determines the start position of player 2

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> assert object1.player1_position == 4
        >>> assert object1.player2_position == 8
        >>> assert object1.player1_score == 0
        >>> assert object1.player2_score == 0
        >>> assert object1.die_roll_count == 0
        >>> assert object1.turn == 1
        """
        super().__init__(player1_start_position, player2_start_position)
        self.three_die_roll_sum_mod_10_dict = {0: 3, 1: 6, 2: 9, 3: 2, 4: 5, 5: 8, 6: 1, 7: 4, 8: 7, 9: 0}

    def __str__(self):
        """
        String representation of this class

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> print(object1)
        Player 1 position: 4
        Player 2 position: 8
        Player 1 score   : 0
        Player 2 score   : 0
        Die rolls        : 0
        Turn             : 1
        Player rolls sum : {0: 3, 1: 6, 2: 9, 3: 2, 4: 5, 5: 8, 6: 1, 7: 4, 8: 7, 9: 0}
        """
        returning_str = super().__str__() + '\n'
        returning_str += "Player rolls sum".ljust(17) + ": " + str(self.three_die_roll_sum_mod_10_dict)
        return returning_str

    def play_next_turn(self):
        """
        The next turn is played adjusting the game state accordingly

        Notes
        -----
        We only need to consider the units place of the summation of the three numbers on the die to move pawn.
        Player 1's die roll units places and the units places of their sum
        1,2,3=6, 7,8,9=4, 3,4,5=2, 9,0,1=0, 5,6,7=8
        Player 2's die roll units places and the units places of their sum
        4,5,6=5, 0,1,2=3, 6,7,8=1, 2,3,4=9, 8,9,0=7

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> player_position, player_score = object1.play_next_turn()
        >>> object1.player1_position = player_position
        >>> object1.player1_score = player_score
        >>> object1.turn = 2
        >>> object1.die_roll_count += 3
        >>> object1.player1_position
        10
        >>> object1.player2_position
        8
        >>> object1.player1_score
        10
        >>> object1.player2_score
        0
        >>> object1.die_roll_count
        3
        >>> object1.turn
        2
        >>> player_position, player_score = object1.play_next_turn()
        >>> object1.player2_position = player_position
        >>> object1.player2_score = player_score
        >>> object1.turn = 1
        >>> object1.die_roll_count += 3
        >>> object1.player1_position
        10
        >>> object1.player2_position
        3
        >>> object1.player1_score
        10
        >>> object1.player2_score
        3
        >>> object1.die_roll_count
        6
        >>> object1.turn
        1
        """
        if self.turn == 1:
            player1_position = self.player1_position + self.three_die_roll_sum_mod_10_dict[
                (self.die_roll_count + 1) % 10] if (self.player1_position + self.three_die_roll_sum_mod_10_dict[
                (self.die_roll_count + 1) % 10]) % 10 == 0 else (self.player1_position +
                                                                 self.three_die_roll_sum_mod_10_dict[
                                                                     (self.die_roll_count + 1) % 10]) % 10
            player1_score = self.player1_score + player1_position
            return player1_position, player1_score
        elif self.turn == 2:
            player2_position = self.player2_position + self.three_die_roll_sum_mod_10_dict[
                (self.die_roll_count + 1) % 10] if (self.player2_position + self.three_die_roll_sum_mod_10_dict[
                (self.die_roll_count + 1) % 10]) % 10 == 0 else (self.player2_position +
                                                                 self.three_die_roll_sum_mod_10_dict[
                                                                     (self.die_roll_count + 1) % 10]) % 10
            player2_score = self.player2_score + player2_position
            return player2_position, player2_score

    def roll_60_times(self):
        """
        The game proceeds 20 turns by rolling the die 60 times and adjusting the game state accordingly
        Note: Only invoke this method when the die roll count is initially a multiple of 60.

        Notes
        -----
        We only need to consider the units place of the summation of the three numbers on the die to move pawn.
        Player 1's die roll units places and the units places of their sum
        1,2,3=6, 7,8,9=4, 3,4,5=2, 9,0,1=0, 5,6,7=8
        Player 2's die roll units places and the units places of their sum
        4,5,6=5, 0,1,2=3, 6,7,8=1, 2,3,4=9, 8,9,0=7

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> player1_score, player2_score = object1.roll_60_times()
        >>> object1.player1_score = player1_score
        >>> object1.player2_score = player2_score
        >>> object1.die_roll_count += 60
        >>> object1.player1_position
        4
        >>> object1.player2_position
        8
        >>> object1.player1_score
        60
        >>> object1.player2_score
        45
        >>> object1.die_roll_count
        60
        >>> object1.turn
        1
        """
        assert self.die_roll_count % 60 == 0
        # Getting start positions
        player1_position = self.player1_position
        player2_position = self.player2_position

        # Initializing local score accumulation
        player1_score_accumulation = 0
        player2_score_accumulation = 0

        # Accumulating player 1 local score over 10 turns.
        # The units place in list below that we get on summing three numbers for 5 turns will be repeated and position
        # will reset to start position after 5 turns so multiply 5 turn score accumulation by 2.
        for first_die_roll in [1, 7, 3, 9, 5]:
            player1_position = player1_position + self.three_die_roll_sum_mod_10_dict[first_die_roll % 10] if \
                (player1_position + self.three_die_roll_sum_mod_10_dict[first_die_roll % 10]) % 10 == 0 else \
                (player1_position + self.three_die_roll_sum_mod_10_dict[first_die_roll % 10]) % 10
            player1_score_accumulation += player1_position

        # Accumulating player 2 local score over 10 turns.
        # The units place in list below that we get on summing three numbers for 5 turns will be repeated but position
        # will NOT reset to start position after 5 turns so loop 10 times instead of 5.
        for _ in range(2):
            for first_die_roll in [4, 0, 6, 2, 8]:
                player2_position = player2_position + self.three_die_roll_sum_mod_10_dict[first_die_roll % 10] if \
                    (player2_position + self.three_die_roll_sum_mod_10_dict[first_die_roll % 10]) % 10 == 0 else \
                    (player2_position + self.three_die_roll_sum_mod_10_dict[first_die_roll % 10]) % 10
                player2_score_accumulation += player2_position

        # Returning new game state
        player1_score = self.player1_score + player1_score_accumulation * 2
        player2_score = self.player2_score + player2_score_accumulation

        return player1_score, player2_score

    def play_game(self):
        """
        Play deterministic die game till one player wins. The winning condition is that a player reaches a score of 1000

        Example:
        >>> object1 = load_data("test.txt", 1)
        >>> object1.play_game()
        >>> assert object1.player2_score == 745
        >>> assert object1.die_roll_count == 993
        """
        # Looping 60 rolls/20 turns/10 turns each player till score does not exceed 1000
        player1_score, player2_score = self.roll_60_times()
        while player1_score < 1000 and player2_score < 1000:
            # Adjusting game state
            self.player1_score = player1_score
            self.player2_score = player2_score
            self.die_roll_count += 60
            player1_score, player2_score = self.roll_60_times()

        assert self.die_roll_count % 60 == 0

        # Looping 3 rolls/1 turn for a player till score does not exceed 1000
        player_position, player_score = self.play_next_turn()
        while player_score < 1000:
            # Adjusting game state
            if self.turn == 1:
                self.player1_position = player_position
                self.player1_score = player_score
                self.turn = 2
            elif self.turn == 2:
                self.player2_position = player_position
                self.player2_score = player_score
                self.turn = 1
            self.die_roll_count += 3
            player_position, player_score = self.play_next_turn()

        # Playing final turn and adjusting game state
        if self.turn == 1:
            self.player1_position = player_position
            self.player1_score = player_score
            self.turn = 2
        elif self.turn == 2:
            self.player1_position = player_position
            self.player1_score = player_score
            self.turn = 1
        self.die_roll_count += 3

        return


def load_data(file_name, code):
    with open(file_name, 'r') as f:
        player1_start_position = int(f.readline().strip().split(' ')[-1])
        player2_start_position = int(f.readline().strip().split(' ')[-1])

    if code == 1:
        return DeterministicDie(player1_start_position, player2_start_position)
    elif code == 2:
        return DiracDie(player1_start_position, player2_start_position, 21)


def main():
    parser = argparse.ArgumentParser(description='AoC Day 21 Dirac Die')
    parser.add_argument('-f', '--file',
                        help='Input file with .',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Product of loser score and die rolls or 2:',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    dirac_die_game_object = load_data(arguments.file, arguments.code)
    # print(dirac_die_game_object)

    if arguments.code == 1:
        assert isinstance(dirac_die_game_object, DeterministicDie)
        dirac_die_game_object.play_game()
        if dirac_die_game_object.player1_score < dirac_die_game_object.player2_score:
            print(dirac_die_game_object.player1_score * dirac_die_game_object.die_roll_count)
        else:
            print(dirac_die_game_object.player2_score * dirac_die_game_object.die_roll_count)
    elif arguments.code == 2:
        assert isinstance(dirac_die_game_object, DiracDie)
        dirac_die_game_object.play_game_driver()
        if dirac_die_game_object.player1_wins > dirac_die_game_object.player2_wins:
            print("Player 1 wins and in " + str(dirac_die_game_object.player1_wins) + " universes")
        else:
            print("Player 2 wins and in " + str(dirac_die_game_object.player2_wins) + " universes")
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
