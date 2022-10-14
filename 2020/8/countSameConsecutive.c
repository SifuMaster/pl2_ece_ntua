/* Run with:
 *
 * frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 15 -wp-verbose 0 countSameConsecutive.c -then -report
 *
 * Tested with Frama-C Vanadium-20210721.
 * Why3 platform, version 1.4.0
 * alt-ergo 2.4.1
 */


/*@ predicate isStreakWithMultitude{L} (integer from, int *x, integer multitude) = 
  @   \forall integer i; from <= i <= from + multitude -1 ==> \at(x[from], L) == \at(x[i], L);
*/

/*@ predicate isBestStreak{L} (int* x, integer max, integer best) = 
  @ (\forall integer from, best_temp;((0 <= from <= max-best) && isStreakWithMultitude(from, x, best_temp)) ==> best >= best_temp);
*/

/*@ predicate bestStreakExists{L} (int* x, integer max, integer best) =
  @   (\exists integer from; ((0 <= from <= max-best) && isStreakWithMultitude(from, x, best)) ==> isBestStreak(x, max, best));
*/

/*@ requires 1 <= N <= 1000000;
  @ requires \valid(x+ (0..N-1));
  @ assigns \nothing;
  @ ensures 1 <= \result <=N;
  @ ensures bestStreakExists(x, N, \result);
*/
int countSameConsecutive(int N, int x[])
{
    int best = 0, i = 0;
    /*@ loop invariant 0 <= i <= N;
      @ loop invariant 0 <= best <= N;
      @ loop invariant i >= best;
      @ loop invariant i > 0 <==> best > 0;
      @ loop invariant bestStreakExists(x, N, best) || (best == 0 <==> i == 0);
      @ loop assigns best, i;
      @ loop variant N-i;
    */
    while (i < N)
    {
        int j = i + 1;
        //@ assert j == i + 1;
        /*@ loop invariant i < j <= N;
          @ loop invariant x[j-1] == x[i];
          @ loop invariant (x[j] == x[i]) ==> isStreakWithMultitude(i, x, j-i+1);
          @ loop assigns j; 
          @ loop variant N-j;      
        */
        while (j < N && x[j] == x[i])
            ++j;
        //@ ghost int best_temp = best; int j_temp = j; int i_temp = i;
        if (j - i > best)
            best = j - i;
        i = j;
        //@ assert best == \max(j_temp - i_temp, best_temp);
        //@ assert i == j;
    }
    return best;
}
