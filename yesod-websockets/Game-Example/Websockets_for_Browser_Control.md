
##BROWSER CONTROL WITH WEBSOCKETS##

**INTRODUCTION**

The Haskell version of the game of *Score* was developed by modifying the chat.hs file at <http://github.com/yesodweb/websockets>. The chat login remains the same, but the chat window is replaced by the real time, multi-player game of score. Websockets are used to provide players with a scoreboard that keeps track of participants and their current scores with:

- no backend database or flatfile
- no persistent entities or transactions
- no cookies


The online browsers add and deduct game points, maintain the list of current players, and keep track of scores by manipulating the values of javascript variables. The Yesod/Warp server does not get involved. It generates random numbers simulating rolls of four dice: two six-sided dice, one twelve-sided die, and one twenty-sided die. Other than that, the server acts mainly as a conduit for messages from individual players to the online group.

The game of *Score* shows how coders familiar with Javascript but new to Haskell can get fairly complex Haskell web applications up and running using the coding techniques with which they are familiar. It might also remind seasoned Haskell programmers that websockets can sometimes be very useful in creating fast, efficient web applications that delegate much or most of the work to the browsers.


**THE ELEGANCE OF JAVASCRIPT**

Here's how the game works: When a player clicks "ROLL", the server sends a fresh set of four random numbers to all participants. If a player sees a way to make the number 20 by means of two or three operations involving any combination of addition, subtraction, multiplication, division, or concatenation, the player can click the button labeled "SCORE" to start a 30-second countdown and try to win a point. Clicking "SCORE" sends a message to the server which is passed on to all browsers setting T = 30. That's all it takes to start the clocks running. Just set the global variable T to anything above 0 and a countdown back to 0 begins. A "setInterval" function cycles once per second, testing the value of T. If T is greater than 0, it sets
```javascript
    T = T - 1
```
and prints the value in the browser. The visible countdown function is not involved in keeping score; that is done by other countdown routings activated by clicking "SCORE" or "IMPOSSIBLE".

Clicking "IMPOSSIBLE" sends a message to all participants (routed through the server) setting T = 60. After 60 seconds, the player who clicked "IMPOSSIBLE" gets one point; but if during the 60-second countdown another player clicks "SCORE", a message is routed to all players setting T = 30. If the player who clicked "SCORE" makes the number 20 before T = 0, that player gains one point and the player who clicked impossible loses one point.

As soon as a player scores, or when a player fails to obtain the number 20 after 30 seconds or three computations, the player's score is increased or decreased and a message goes out to all browsers announcing the outcome and setting T = 0 so another round can begin.

In other words, a player who clicks "SCORE" loses a point if 30 seconds elapse or if three computations fail to produce 20, and gains a point if 20 is computed within the time limit. A player who clicks "IMPOSSIBLE" gains a point unless another player clickes "SCORE" and computes the number 20 within 30 seconds. Individual scores are maintained in each player's browser in the variable "score"; but almost immediately, all of the scoreboards on all participating browsers see the update. Thanks to Javascript's mutable variables, updates are accomplished simply and elegantly as follows:

Every three seconds, each player sends his or her name and score to the server which immediately broadcases the information to all participating browsers. The name and score are placed in a javascript object in the following format:

``` javascript
{"player" : {"player",score}}
```
Each browser has an object called "game". When a player/score object arrives, say
```javascript 
{"Fred" : {"player":"Fred, "score":42}}
```
the code
```javascript
game["Fred"] = {"Fred" : {"player":"Fred, "score":42}}
```
is executed. As with "T", the mutable global variable "game" provides an efficient way to maintain identical, updated scoreboards on all browsers. Javascript objects cannot have duplicate keys, so it doesn't matter if Fred was already in game, perhaps with a different score. The new Fred object bumps the older one out of the way and the next scoreboard update shows the result.

**Browser disconnects**

Disconnects are noted without parsing Internet protocol packets or interacting with session cookies. There are no session cookies, and the server does not participate in maintaining up-to-date scoreboards. The browsers take care of this with the following code:
```javascript
        setInterval (function () {
            game = {};
        }, 10000);
```
Every 10 seconds, on each browser, all players are deleted from game. The game object is soon replenished with the names and scores of all participants because each broser refreshes game every 3 seconds using the code
```javascript
        setInterval (function () {
            conn.send("COWS," + player + "," + score);
        }, 3000);
```
When browsers receive messages beginning with "COWS", they react as follows:
```javascript
        if (data.substring(0,4) == "COWS") {
            var x = data;
            var l = x.split(",");          // creates an array
            var pl = l[1];
            var sc = l[2];
            game[pl] = {"player" : pl, "score" : sc};
            players[pl] = pl
        }
```
Notice that the object "players" gets only the player's name, but not the score. The "players" object is used to screen attempts to log in with names that are already taken. The first four lines inside the if clause split the incoming string into a three-member list and attach the player and the score to the object "game". Mutable global variable allow much to be accomplished with few lines of code. The server could participate in time keeping and scoreboard updating but MVars, the state monad, stm, or some such way of avoiding side effects seems like a lot of work compared with the simple elegance of using the mutable global variables "T" and "game". For production code, names such as "SCH_T" and "SCH_game" might be used to avoid inadvertant clashes with someone else's code using the global variables "T" and "game". the prefix "SCH" could be used to create a namespace for code written by Schalk.

The code for the game of **Score** is pretty straghtforward. One part that might need an explanation is the code involving a click of the "IMPOSSIBLE" button. When that happens, clickers send their names to all the other browsers (through the server) as follows:
```javascript
        $("#IMPOSSIBLE").click( function () {
            score = score - 1;
            T3 = 61;
            localCalc2();
            conn.send("SIXTY" + player);
        });
```

One point is immediately deducted from the clicker's score, but two points will be added if the timer reaches T3 = 0, or if someone clicks "SCORE" and either can't show a solution within 30 seconds or performs 3 computations which don't result in the number 20. T3 is set for only the clicker's browser. The local function localCalc2 is calledas follows:
```javascript
        var localCalc2 = function() {
          setTimeout (function () {
            T3 = T3 - 1;
            if (T3 > 0) {
                localCalc2();
            }
            if (T3 == 0) {
                score += 2;
                return;
            }
            else return;           // not necessary
          }, 1000)
        }
```
Clicking "IMPOSSIBLE" caused the string ("SIXTY" + player) to be sent to the server and broadcast to all participants. Each browser intercepts the string as follows:
```javascript
            if (data.substring(0,5) == "SIXTY") {
                T = 61;
                var name = data.substring(5);
                $("#SCORE3").show();
                $("#roll").hide();
                $("#submitB").hide();
                $("#SCORE1").hide();
                $("#IMPOSSIBLE").hide();
                $("#output2").show(); 
                $("#output2").html("The deduction from " + name + "'s score might be temporary.")
                impossiblePlayer = e.data.substring(5); 
                $("#output4").html("impossiblePlayer is " + impossiblePlayer);        
            }
```
T = 61 sets the countdown in all of the browsers. The rest of the code is just display housekeeping. Clicking "SCORE" will reset the countdown to 30 with the simple instruction
```javascript
     T = 31
```
The reason the countdown timer can be set and re-set by changing the value of T is this perpetually (up to the timeout limit) cycling code:

```javascript
        SCH_t = 3600
        setInterval (function () {
            SCH_t = SCH_t - 1;
            var timer = function () {
                if (T < 1) {
                    return;
                } else {
                    T = T - 1;
                    $("#output").html(T);
                }
            }
            timer();
        }, 1000);
```
Once every second, setInterval checks to see if T is greater than 0. If it is, it deminishes T by 1 and displays its value in the browser. This is only for display. Scorekeeping depends on T2, T3, and T4, which are not shown in the browsers.

If someone clicks "SCORE" during the 60 countdown, T3 = (-1) is set, stopping the local, invisible *Impossible* countdown and starting localCalc3 as follows:
```javascript
        $("#SCORE3").click( function () {
            $("#submitA").show();
            conn.send("THIRTY2");
            T4 = 31;
            localCalc3();
        });
```
Hhere is localCalc3:
```javascript
        var localCalc3 = function() {
          setTimeout (function () {
            T4 = T4 - 1;
            if (T4 > 0) {
                localCalc3();
            }
            else if (T4 == 0) {
                score -= 1;
                return;
            } 
            else {
                return;
            }
          }, 1000)
        }
```
The clicker will lose a point if the countdown is not interrupted by computing the number 20. Here is how the interruption occurs:
```javascript
        var firstCalc = function (a,b,c) {
            var res;
            switch (b) {
                case 0: res = a + c;
                break;
                case 1: res = a - c;
                break;
                case 2: res = a * c;
                break;
                case 3: res = a / c;
                break;
                case 4: res = a+""+c
            }
            return res;
        };

        var calculate = function (a,b,c) {
            var res;
            switch (b) {
                case 0: res = a + c;
                break;
                case 1: res = a - c;
                break;
                case 2: res = a * c;
                break;
                case 3: res = a / c;
                break;
                case 4: res = a+""+c
            }
            if (res == 20) {
                score += 1;
                conn.send("WINNER" + player)
            }
            return res;
        };
```
Players don't get credit for computing 20 in the first round, so firstCalc does not affect the score. But calculate has the block:
```javascript
            if (res == 20) {
                score += 1;
                conn.send("WINNER" + player)
            }
```
The player's score is increased if the computation result is 20, in which case the string ("WINNER" + player) goes to the server and is broadcast. Browsers intercept the message with:
```javascript
            if (data.substring(0,6) == "WINNER") {
                T = 0;
                T2 = (-1)
                T4 = (-1)
                var win = data.substring(6);
                $("#roll").show();
                $("#submitA").hide();
                $("#out4").hide();
                $("#out5").hide();
                $("#out6").hide();
                winner(win);
            }
```
T = 0 resets the display clocks. Any invisible timers which are running get deactivated, the ROLL button is displayed, some display housekeeping is performed, and winner(win) is called. Here is the function "winner".
```javascript
        var winner = function (x) {
            $("#output").html("");
            $("#output2").html("<h2>One point for " + x + "</h2>");
        }            if (data.substring(0,6) == "WINNER") {
                T = (-1);
                T2 = (-1);
                T3 = (-1);
                T4 = (-1);
                var win = data.substring(6);
                $("#roll").show();
                $("#submitA").hide();
                $("#submitB").hide();
                $("#submitC").hide();
                $("#submitD").hide();
                $("#SCORE3").hide();
                winner(win);                   
            }                   
```
It just announces the result, displays the "ROLL" button, and hides some useless buttons which might otherwise be displayed, and calls the function "winner". Here is the function "winner".
```javascript
        var winner = function (x) {
            $("#output").html("<h2>One point for " + x + "</h2>");
            $("#output2").html("");
        }
```
It just announces the result and erases whatever useless information remains in the div with id = "output2".

Clicking "IMPOSSIBLE" caused
```javascript
        conn.send("SIXTY" + player);
```
to be executed, causing the server to broadcast the string "SIXTY" + player. Each browser uses the string as follows:
```javascript
        if (data.substring(0,5) == "SIXTY") {
            T = 61;
            impossiblePlayer = data.substring(5);
            $("#output2").html("The deduction from " + player + "'s score might be temporary.");
            $("#submitB").hide();
            $("#output2").show();
            $("#SCORE3").show();
            $("#SCORE1").hide();
            $("#output2").html("");
```
The mutable global variable "impossiblePlayer" is set to the name of the player who clicked "IMPOSSIBLE". If someone clickes "SCORE", impossiblePlayer is used in the following code:
```javascript
             if (data.substring(0,6) == "LOSER2") { 
                T = -1;
                T4 = -1;
                var lose = e.data.substring(6);
                if (player == impossiblePlayer) {
                    score += 2;
                }
                $("#output2").html("");
                $("#roll").show();
                $("#submitA").hide();
                $("#submitB").hide();
                $("#submitC").hide();
                $("#submitD").hide();
                $("#SCORE3").hide();
                loser2(lose);
            }
```
This shows some of the things that happen if someone clicks "SCORE" after someone else clicked "IMPOSSIBLE". The player who clicked "IMPOSSIBLE" gets two points, more than compensating for the initial one point deduction. The function "loser2" is called, causing a message to be displayed as follows:
```javascript
        var loser2 = function (x) {
            $("#output").html("");
            $("#output3").html("<h2>Deduct one point from " + x + "<br>One point for " + impossiblePlayer + "</h2>");     
        }
```
The variable "impossiblePlayer" that was set in each browser when someone clicked "IMPOSSIBLE" is used to announce the outcome of the completed round.








