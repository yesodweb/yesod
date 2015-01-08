
##BROWSER CONTROL WITH WEBSOCKETS##

**INTRODUCTION**

The Haskell version of the game of *Score* was developed by modifying the chat.hs file at <http://github.com/yesodweb/websockets>. The chat login remains the same, but the chat window is replaced by the real time, multi-player game of score.
*Score* is definitely not an example of the recommended way to construct a Yesod website. It lacks type safety, and would not be a secure way to handle sensitive data.
"Score" demonstrates the use of websockets to facilitate interactions among web browsers. Beyond that, it can be an amusing solitaire or multiplayer game, and can also serve as a learning tool for use in elementary schools. Players can practice subtracting negative numbers and dividing by fractions, and they can see these operations performed by clicking "EVALUATE" to see all possible solutions.

**HOW THE SCOREBOARDS ARE MAINTAINED**

Let's begin by examining how *Score* provides players with a scoreboard that keeps track of participants and their current scores. Then we will see how players gain or lose points.

The online browsers add and deduct game points, maintain identical listS of current players, and keep track of scores by manipulating the values of javascript global variables. Global variables can lead to name clashes, so the prefix "DS_" is affixed to all global variables, creating a namespace for developer DS. If other coders add global variables, they should use their own unique prefixes.

Each player keeps his nickname and score in the global variables "DS_player" and "DS_score". These are routed through the server to all participants once every 3 seconds with the following code:
``` javascript
        setInterval (function () {
            conn.send("COWS," + DS_player  + "," + DS_score);
        }, 1000);
```
The websockets server receives the message and broadcasts it. Each browser intercepts the message, parses it, and sets the global scoreboard variable as follows:
``` javascript
    if (e.data.substring(0,4) == "COWS") {
        var x = e.data;
        var l = x.split(",");
        var pl = l[1];
        var sc = l[2];
        DS_game[pl] = {"player" : pl, "score" : sc};
        DS_players[pl] = pl
    }
```
The global variable "DS_game" is an object containing objects of the form:
``` javascript
DS_game["Steve"] = {"Steve" {"player":Steve, "score":42}}
```
Duplicate indeces are not permitted in Javascript, so any "Steve" component of DS_game gets replaced by the new addition. The following perpetually running code makes sure that only current players are listed on the scoreboard:
``` javascript
    setInterval (function () {
        DS_game = {};
    }, 10000);
```
DS_game is wiped clean every 10 seconds, but repleshed by each browser every 3 seconds. Each player sees a complete and up-to-date scoreboard.

The object "DS_players" gets only the player's name, but not the score. The "DS_players" object is used to screen attempts to log in with names that are already taken.

**HOW POINTS ARE GAINED AND LOST**

Here's how the game works: When a player clicks "ROLL", the server sends a fresh set of four random numbers to all participants. If a player sees a way to make the number 20 by means of two or three operations involving any combination of addition, subtraction, multiplication, division, or concatenation, the player can click the button labeled "SCORE" to start a 30-second countdown and try to win a point. Clicking "SCORE" sends a message to the server which is passed on to all browsers setting DS_T = 30. That's all it takes to start the clocks running. Just set the global variable T to anything above 0 and a countdown back to 0 begins. A setInterval function cycles once per second, testing the value of T. If T is greater than 0, it sets T = T - 1 and prints the value in the browser. The visible countdown function is not involved in keeping score; that is done by other countdown routines activated by clicking "SCORE" or "IMPOSSIBLE".

Clicking "IMPOSSIBLE" sends a message to all participants (routed through the server) setting DS_T = 60. After 60 seconds, the player who clicked "IMPOSSIBLE" gets one point; but if during the 60-second countdown another player clicks "SCORE", a message is routed to all players setting T = 30. If the player who clicked "SCORE" makes the number 20 before DS_T = 0, that player gains one point and the player who clicked impossible loses one point.

As soon as a player scores, or when a player fails to obtain the number 20 after 30 seconds or three computations, the player's score is increased or decreased and a message goes out to all browsers announcing the outcome and setting DS_T = 0 so another round can begin.

The code for the game of **Score** is pretty straghtforward. One part that might need an explanation is the code involving a click of the "IMPOSSIBLE" button. When that happens, clickers send their names to all the other browsers (through the server) as follows:
``` javascript
    $("#IMPOSSIBLE").click( function () {
        TOG = "on";
        DS_score = DS_score - 1;
        conn.send("SIXTY" + DS_player);
        T3 = 61;
        localCalc2();
    });
    });
```

One point is immediately deducted from the clicker's score, but two points will be added if the timer reaches T3 = 0, or if someone clicks "SCORE" and either can't show a solution within 30 seconds or performs 3 computations which don't result in the number 20. T3 is set for only the clicker's browser. The local function localCalc2 is called as follows:
```javascript
    var localCalc2 = function() {
      setTimeout (function () {
        T3 = T3 - 1;
        if (DS_T3 > 0) {
            localCalc2();
        }
        if (DS_T3 == 0) {
            DS_score += 2;
            conn.send("WINNER" + DS_player);
            return;
        }
        else return;
      }, 1000)
    }
```
Clicking "IMPOSSIBLE" caused the string ("SIXTY" + DS_player) to be sent to the server and broadcast to all participants. Each browser intercepts the string as follows:
```javascript
    if (data.substring(0,5) == "SIXTY") {
        DS_T = 61;
        var name = data.substring(5);
        $("#SCORE3").show();
        $("#roll").hide();
        $("#submitB").hide();
        $("#SCORE1").hide();
        $("#IMPOSSIBLE").hide();
        $("#output2").show();
        $("#output2").html("The deduction from " + name + "'s DS_score might be temporary.")
        DS_impossiblePlayer = e.data.substring(5);
        $("#output4").html("impossiblePlayer is " + DS_impossiblePlayer);
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
            if (DS_T < 1) {
                return;
            } else {
                DS_T = DS_T - 1;
                $("#output").show();
                $("#output").html(DS_T);
            }
        }
        timer();
    }, 1000);
```
Once every second, setInterval checks to see if T is greater than 0. If it is, it deminishes T by 1 and displays its value in the browser. This is only for display. Scorekeeping depends on T2, T3, and T4, which are not shown in the browsers.

If someone clicks "SCORE" during the 60 countdown, T3 = (-1) is set, stopping the *Impossible* countdown in the "IMPOSSIBLE" clicker's browser, and starting localCalc3 as follows:
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
        T3 = (-1)
        T4 = T4 - 1;
        if (DS_T4 > 0) {
            console.log(T4);
            localCalc3();
        }
        else if (DS_T4 == 0) {
            DS_score -= 1;
            conn.send("LOSER2" + DS_player);
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
                conn.send("WINNER" + DS_player)
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
The player's score is increased in the player's browser. A message is routed through the server and broadcast to all participating browsers where the message is parsed and used as follows:

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
            $("#output").html("<h2>One point for " + x + "</h2>");
            $("#output2").html("");
        }
```
It just announces the result and erases whatever useless information remains in the div with id = "output2".

