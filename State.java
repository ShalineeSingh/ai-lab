//Greedy algorithm implementation//
//Definition of how the state changes// 


import java.util.Arrays;

class State {

  /** The array representing the puzzle's state. */
  public int[] array = new int[9];

  /** The index location of the blank tile in the current state. */
  public int blankIndex;

  /** The number of moves since the start. */
  private int g;

  /** The number of moves to the goal. */
  private int h;

  /** The previous state. */
  private State previous;

  public State(int[] input) {
    this.array = input;
    this.blankIndex = getIndex(input, 0);
    this.previous = null;
    this.g = 0;
    this.h = Puzzle.getgreedyHeuristic(this.array);
    
  }

  public State(State previous, int blankIndex) {
    this.array = Arrays.copyOf(previous.array, previous.array.length);
    this.array[previous.blankIndex] = this.array[blankIndex];
    this.array[blankIndex] = 0;
    this.blankIndex = blankIndex;
    this.g = previous.g + 1;
   
    this.h = Puzzle.getHeuristic(this.array);
    this.previous = previous;
  }

  public static int getIndex(int[] array, int value) {
    for (int i = 0; i < array.length; i++)
      if (array[i] == value) return i;
    return -1;
  }

  public boolean isSolved() {
    int[] p = this.array;
    for (int i = 1; i < p.length - 1; i++)
      if(p[i-1] > p[i]) return false;

    return (p[0] == 1);
  }

  public String toString() {
    int[] state = this.array;
    String s = "\n\n";
    for(int i = 0; i < state.length; i++) {
      if(i % 3 == 0 && i != 0) s += "\n";
      s += (state[i] != 0) ? String.format("%d ", state[i]) : "  ";
    }
    return s;
  }

  public String allSteps() {
    StringBuilder sb = new StringBuilder();
    if (this.previous != null) sb.append(previous.allSteps());
    sb.append(this.toString());
    return sb.toString();
  }

  public String solutionMessage(long startTime) {
    long solveTime = System.currentTimeMillis() - startTime;
    StringBuilder sb = new StringBuilder();
    sb.append("Here are the steps to the goal state:");
    sb.append(this.allSteps());
    sb.append("\n\nGiven puzzle is SOLVED!");
    sb.append("\nSolution took " + solveTime + "ms and " + this.g + " steps.\n");
    return sb.toString();
  }

  public int g() {
    return this.g;
  }
  public int h() {
    return this.h;
  }

  public int f() {
    return g() + h();
  }

  public State getPrevious() {
    return this.previous;
  }

}
