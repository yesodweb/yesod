{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
import Yesod.Core
import Yesod.WebSockets
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Conduit
import Data.Monoid ((<>))
import Control.Concurrent.STM.Lifted
import Data.Text (Text, pack, isPrefixOf)
import Fm hiding (main) 
import Text.Julius

go = "GO" :: Text
greeting = "The game of Score!" :: Text
solutions = "SOLUTIONS" :: Text

data App = App (TChan Text)

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

scoreApp :: WebSocketsT Handler ()
scoreApp = do 
    App writeChan <- getYesod
    readChan <- atomically $ do 
        writeTChan writeChan greeting
        dupTChan writeChan
    -- ps <- liftIO new
    race_ 
        (forever $ atomically (readTChan readChan) >>= sendTextData) 
        (sourceWS $$ mapM_C (\msg -> 
          if msg == go 
            then do 
              let x = roll 6 6 12 20
              y <- liftIO $ truck x
              let z = map round x
              liftIO $ atomically $ writeTChan writeChan
                $ go <> pack (show z) 
              liftIO $ atomically $ writeTChan writeChan 
                $ solutions <> pack y 
          else 
              liftIO $ atomically $ writeTChan writeChan msg )) 

getHomeR :: Handler Html
getHomeR = do 
    webSockets scoreApp
    defaultLayout $ do
        [whamlet|
        <html>
          <div id="left">
            <head>
                <script src="//ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js"></script>
            <div #output>
            <div #output2>
            <div #output3>

            <button type="submit" class="score1" id="SCORE1">SCORE</button>
            <button type="submit" class="score1" id="SCORE3">SCORE</button>           
            <button type="submit" class="score1" id="IMPOSSIBLE">IMPOSSIBLE</button>
            <button type="submit" class="score1" id="roll">ROLL</button>

            <form #form>
                <input #input>
            <div #out2>

            <div #out3> 
                            
                    <input type="radio" name="day" id="x0"  checked> 
                    <label for="x0" class="radio"><span id="0">blank</span></label>
              
                    <input type="radio" name="day" id="x1"  > 
                    <label for="x1" class="radio"><span id="1">blank</span></label>
              
                    <input type="radio" name="day" id="x2" > 
                    <label for="x2" class="radio"><span id="2">blank</span></label>
             
                    <input type="radio" name="day" id="x3" > 
                    <label for="x3" class="radio"><span id="3">blank</span></label>
                             
                    <br>
                    <input type="radio" name="ops" value="0" id="x4" checked>
                    <label for="x4" class="radio"><span id="4">blank</span></label> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            
                    <input type="radio" name="ops" value="1" id="x5">
                    <label for="x5" class="radio"><span id="5">blank</span></label> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              
                    <input type="radio" name="ops" value="2" id="x6">
                    <label for="x6" class="radio"><span id="6">blank</span></label> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            
                    <input type="radio" name="ops" value="3" id="x7"> 
                    <label for="x7" class="radio"><span id="7">blank</span></label> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
     
                    <input type="radio" name="ops" value="4" id="x8">    
                    <label for="x8" class="radio"><span id="8">blank</span></label> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    <br>
                               
                    <input type="radio" name="night" id="x9" checked> 
                    <label for="x9" class="radio"><span id="9">blank</span></label>
          
                    <input type="radio" name="night" id="x10" > 
                    <label for="x10" class="radio"><span id="10">blank</span></label>
            
                    <input type="radio" name="night" id="x11" > 
                    <label for="x11" class="radio"><span id="11">blank</span></label>
           
                    <input type="radio" name="night" id="x12" > 
                    <label for="x12" class="radio"><span id="12">blank</span></label>
    
                    <br>
                <button type="submit" class="score1" id="submitA">EVALUATE</button>
                <button type="submit" class="score1" id="submitB">EVALUATE</button>
                <button type="submit" class="score1" id="submitC">EVALUATE</button>

            <div #out7> 
                <br><br>
                <button type="submit" class="score1" id="sol">SOLUTIONS</button> 
                <button type="submit" class="score1" id="erase">ERASE</button> 
            <div #out8> 
                <h3>All Solutions</h3>      
            <div id="out9"><button type="submit" class="score1" id="erase2">ERASE</button></div>
            
          <div id="right">
            <h2>Score Board
            <div id="rightOne">
        |]
        toWidget [lucius|
            body {
                background-color: black;
                color: yellow;
                font-family: calibri;
            }

            button {
               font-size : 16px;
            }

            button:hover {
               font-size : 20px;
            }

            #left {
                float: left;
                width: 60%;
            }
                          
            #right {
                float: right;
                width:30%;
            }

            #output {
                margin-bottom: 1em;
                p {
                    margin: 0 0 0.5em 0;
                    padding: 0 0 0.5em 0;
                    border-bottom: 1px dashed #99aa99;
                }
            }

            input[type=submit] {
                    display:none;
                }

            input[type=submit]  + label {
                    display:inline-block;
                    font-size: 30px;
                    text-align: center;
                    vertical-align: middle;
                    cursor: pointer;
                }

            input[type=submit] :checked + label {
                        font-size: 40px;
                        color: #ff0000;
                }

            input[type=submit] :hover + label:hover {
                        color: #ff0000;
                        font-size: 40px;
                }

            input[type=radio] {
                    display:none;
                }

            input[type=radio] + label {
                    display:inline-block;
                    font-size: 40px;
                    text-align: center;
                    vertical-align: middle;
                    cursor: pointer;
                }

            input[type=radio]:checked + label {
                        font-size: 40px;
                        color: #ff0000;
                }

            input[type=radio]:hover + label:hover {
                        color: #ff0000;
                        font-size: 40px;
                }

            .score1 {
                    background-color:transparent;
                    color:#abb4cc;
                    text-align: center;
                    vertical-align: middle;
                    font-style:normal;
                    height:40px;
                    line-height:35px;
                    width:120px;
                }.score1:active {
                    position:relative;
                    // top:1px;
            }
            
        |]
        toWidget [julius|


// ***************************************   timer control variables   ***

        var DS_T;    // Used for countdown in DS_score and IMPOSSIBLE.
        var DS_T2;   // Used for private countdowns in the individual browser. The file is countDown
        var DS_T3;   // Used for private countdowns. The file is countDown2. This is for DS_score after IMPOSSIBLE.
        var DS_T4;   // Used for countdown in DS_score after IMPOSSIBLE has been clicked.

// *************************************  end timer control variables  ***
        var DS_a;
        var DS_b;
        var DS_c;
        var DS_d;
        var DS_e;
        var DS_player  = "Default Player";
        var DS_impossiblePlayer = "Bozo the Clown";
        var DS_score = 0; 
        var DS_solutions = "";
        var DS_players = {};
        var DS_game = {"Steve" : {"DS_player" : "Steve", "DS_score" : 42}};
        var DS_ops = [" + ", " - ", " * ", " / ", "Concat"];
        var url = document.URL,
            form = document.getElementById("form"),
            input = document.getElementById("input"),
            conn;
            url = url.replace("http:", "ws:").replace("https:", "wss:");
            conn = new WebSocket(url);

        var TOG = "off";

// ***********************************   Page setup   ***

        $(document).ready( function () {
            $("#output2").html("To get started, please enter a nickname.");   
            $("#roll").hide();
            $("#SCORE3").hide();
            $("#SCORE1").hide();
            $("#submitA").hide();
            $("#submitB").hide();
            $("#submitC").hide();
            $("#submitD").hide();
            $("#out3").hide();
            $("#out7").hide();
            $("#out8").hide();
            $("#out9").hide();

            $("#SCORE1").hide();
            $("#IMPOSSIBLE").hide();
            $("#SCORE3").hide();

            $("#4").val("+");
            $("#5").val("-");
            $("#6").val("*");
            $("#7").val("/");
            $("#8").val("concat");
        });

// $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FUNCTIONS  


        function elep() {
            var selDay2 = document.getElementsByName('day');
            var selOps2 = document.getElementsByName('ops');
            var selNight2 = document.getElementsByName('night');
            var y = {};
            var cow = [];
            var a;
            var b;

            for (var i = 0; i < 4; i+=1) {
                if (!(selDay2[i].checked || selNight2[i].checked)) {
                    cow.push(selDay2[i].value);
                }
            }

            for (var i = 0; i < 4; i++) {
                if (selDay2[i].checked) {
                    a = i;
                    y.a = 1 * selDay2[i].value;
                }
            }

            for (var i = 0; i < 5; i++) {
                if (selOps2[i].checked) {
                    y.b = 1 * selOps2[i].value;
                }
            }

            for (var i = 0; i < 4; i++) {
                if (selNight2[i].checked) {
                    c = i;
                    y.c = 1 * selNight2[i].value;
                }
            }

            if (a == b) {
                $("#output3").html("<h2>YOU CAN'T USE THE SAME DIE TWICE");
                return;
            }

            else if (a != b) {
                DS_a = y.a;
                DS_b = y.b;
                DS_c = y.c;
                DS_d = cow[0];
                DS_e = cow[1];
            }
        }




// *****************************************************   Perpetually cycling functions   ***

        setInterval (function () {
            conn.send("COWS," + DS_player  + "," + DS_score);                           
        }, 1000);

        setInterval (function () {
            DS_game = {};                           
        }, 10000);

        setInterval (function () {
            $("#rightOne").html("");
            for (players in DS_game) {
                $("#rightOne").append(DS_game[players].player + " " + DS_game[players].score + "<br>");
            }                               
        }, 500);

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

// *********************************************************   Sign-in screener   ***

        var f9 = function (el) {
            var x;
            for (cows in DS_players) {
                if (players[cows] == el) {x = true;} else {x = false;}
            };
            if (x) {$("#output2").html("<h2>" + el + " is taken. Please try again.");
            } else {
                conn.send("COWS" + el + 0);
                DS_player  = el;
                $("#form").hide();
                $("#roll").show();
            }
        } 

// ^^^^^^^^^^^^^^^^^^^^^^^^^^^   DS_player  clicked:  SCORE, IMPOSSIBLE, or SCORE2   ^^^

        var localCalc1 = function() {
          setTimeout (function () {
            DS_T2 = DS_T2 - 1;
            if (DS_T2 > 0) {
                localCalc1();
            }
            if (DS_T2 == 0) {
                DS_score -= 1;
                conn.send("LOSER" + DS_player)
                return;
            } 
            else return;     
          }, 1000)
        }

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

// **************************************************************   DS_score keeping   ***

        var winner = function (x) {
            $("#output").html("");
            $("#output2").html("<h2>One point for " + x + "</h2>");
        }

        var loser = function (x) {
            $("#output").html("");
            $("#output3").html("<h2>Deduct one point from " + x + "</h2>");     
        }

        var loser2 = function (x) {
            $("#output").html("");
            $("#output3").html("<h2>Deduct one point from " + x + "<br>One point for " + DS_impossiblePlayer + "</h2>");     
        }      

// **********************************************    Click callback functions    ***

        $("#roll").click( function () {
            conn.send("GO");
        });

        $("#SCORE1").click( function () {  
            $("#submitA").show();
            conn.send("THIRTY");
            DS_T2 = 31;
            $("#output2").html("");
            localCalc1();
        });

        $("#submitA").click( function () {
            $("#output").html("");
            elep();
            var x = elephant();
            if (x != "cow") {
                conn.send("EVAL_A," + x);
                conn.send("ONE");
                $("#submitB").show();
                $("#submitA").hide();
            }
        });

        $("#submitB").click( function () {
            $("#output").html("");
            elep();
            var x = elephant2();
            if (x != "cow") {
                conn.send("EVAL_A," + x);
                conn.send("TWO");
                $("#submitC").show();
                $("#submitB").hide();
            }
        });

        $("#submitC").click( function () {
            $("#output").html("");
            elep();
            var x = elephant3();
            if (x != "cow") {
                conn.send("EVAL_A," + x);
                conn.send("THREE");
            }
        });

        $("#IMPOSSIBLE").click( function () {
            TOG = "on";
            DS_score = DS_score - 1;
            conn.send("SIXTY" + DS_player);
            T3 = 61;
            localCalc2();
        });           

        $("#SCORE3").click( function () {        // After "Impossible"
            $("#submitA").show();
            conn.send("THIRTY2");
            T4 = 31;
            localCalc3();
        });

// ***************************************************************   DS_solutions   ********** 
        $("#sol").click( function () { 
            $("#out8").append(DS_solutions);    
            conn.send("LOOKED" + DS_player);           
        });

        $("#erase").click( function () { 
            $("#out8").html("");
        });

        $("#erase2").click( function () { 
            $("#out8").html("");
        });

// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   Computation preparation  ^^^ 

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
                conn.send("WINNER" + DS_player)
            } 
            return res;
        };

// *****************************   Number crunching   ***

       function elephant() {
            var a = DS_a;
            var b = DS_b;
            var c = DS_c;
            var d = firstCalc(DS_a, DS_b, DS_c)
            var gak = [DS_a, DS_b, DS_c, d, DS_d, DS_e];
            $("#output3").show();
            $("#output3").append("gak: " + gak + "<br>");
            $("#submitB").show();
            return gak;
        }

       function elephant2() {
            var a = DS_a;
            var b = DS_b;
            var c = DS_c;
            res = calculate(DS_a, DS_b, DS_c);
            var gak = [DS_a, DS_b, DS_c, res, DS_d];
            $("#output3").append(gak + "<br>")
            $("#submitD").show();
            if (res === 20) { 
                DS_score += 1; 
                conn.send("WINNER" + DS_player); 
                return gak;               
            }
            else {return gak};
        }

       function elephant3() {
            var a = DS_a;
            var b = DS_b;
            var c = DS_c;
            var rs = calculate(DS_a, DS_b, DS_c);
            var gak = [DS_a, DS_b, DS_c, rs]; 
            $("#output3").append(gak + "<br>")
            $("#submitD").hide(); 

            if (rs !== 20 && TOG == "off") { 
                DS_score -= 1;
                conn.send("LOSER" + DS_player) 
                return gak; 
            }

             if (rs !== 20 && TOG == "on") { 
                DS_score -= 1;
                conn.send("LOSER2" + DS_player) 
                return gak; 
            }               

            if (rs === 20) { 
                DS_score += 1; 
                conn.send("WINNER" + DS_player); 
                return gak; 
            }
        }

// **************************  PROCESS INCOMING DATA  ***

        conn.onmessage = function(e) {
            var data = e.data;
            var p = document.createElement("p");
            p.appendChild(document.createTextNode(e.data)); 

            if (e.data.substring(0,4) == "COWS") {
                var x = e.data;
                var l = x.split(","); 
                var pl = l[1];
                var sc = l[2]; 
                DS_game[pl] = {"player" : pl, "score" : sc};
                DS_players[pl] = pl 
            }   

            if (e.data.substring(0,9) == "SOLUTIONS") {
                var a = e.data;
                DS_solutions = a.substring(9);
            }   

            if (data.substring(0,2) == "GO") {
                TOG = "off";
                $("#out7").show();
                $("#out8").show();
                $("#out9").show();
                $("#erase2").show();
                $("#output3").html("");
                DS_T = 0;
                DS_T2 = (-1);
                T3 = (-1);
                T4 = (-1);
                $("#1").show();
                $("#2").show();
                $("#3").show();
                $("#10").show();
                $("#11").show();
                $("#12").show();
                $("#output2").html("Click 'SCORE' to begin calculating, "
                    + "<br> or 'IMPOSSIBLE' if you think competitors "
                        + "can't find a solution within 60 seconds. ");
                $("#out2").html("");
                $("#out3").show();               
                $("#SCORE1").show();
                $("#SCORE3").hide()
                $("#IMPOSSIBLE").show();        
                var l = data.substring(3);
                ls = l.split(",");
                $("#output4").html(ls + "<br>");
                var a = ls[0]
                var b = ls[1]
                var c = ls[2]
                var d = ls[3]
                $("#0").html(a + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#1").html(b + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#2").html(c + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#3").html(d)
                $("#4").html("+");
                $("#5").html("-");
                $("#6").html("*");
                $("#7").html("/");
                $("#8").html("concat");
                $("#9").html(a + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#10").html(b + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#11").html(c + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#12").html(d)

                $("#x0").val(a);
                $("#x1").val(b);
                $("#x2").val(c);
                $("#x3").val(d);
                $("#x9").val(a);
                $("#x10").val(b);
                $("#x11").val(c);
                $("#x12").val(d); 
            } 

            if (data == "ONE") {
                $("#3").hide();
                $("#12").hide();
            }

            if (data == "TWO") {
                $("#2").hide();
                $("#11").hide();
            }

            if (data == "THREE") {
                $("#1").hide();
                $("#10").hide();
            }

            if (data.substring(0,6) == "EVAL_A") { 
                var zx = e.data.split(",");
                var a = zx[1]
                var b = zx[2]
                var c = zx[3]
                var d = zx[4]
                var e = zx[5]
                var f = zx[6]
                $("#out2").show();
                $("#out2").append(a + " " + DS_ops[b] + " " + c + " = " + d + "<br>");

                $("#0").html(d + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#1").html(e + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#2").html(f + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#4").html("+");
                $("#5").html("-");
                $("#6").html("*");
                $("#7").html("/");
                $("#8").html("concat");
                $("#9").html(d + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#10").html(e + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                $("#11").html(f + " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")

                $("#x0").val(d);
                $("#x1").val(e);
                $("#x2").val(f);
                $("#x9").val(d);
                $("#x10").val(e);
                $("#x11").val(f);
            }

            if (data == "THIRTY") {
                DS_T = 31;
                $("#IMPOSSIBLE").hide();                    
                $("#SCORE1").hide(); 
                $("#roll").hide(); 
                $("#output2").html("");
            }

            if (data.substring(0,6) == "LOOKED") {
                var name = data.substring(6);
                $("#output2").show();
                $("#output2").html("<br>" + name + " looked at the solutions");
            }

            if (data == "THIRTY2") {
                DS_T = 31;
                T3 = (-1);
                $("#IMPOSSIBLE").hide();
                $("#SCORE3").hide();   
                $("#roll").hide();   
            }

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

            if (data.substring(0,6) == "WINNER") {   // The browsers keep score
                DS_T = (-1);
                DS_T2 = (-1);
                DS_T3 = (-1);
                DS_T4 = (-1);
                var win = data.substring(6);
                $("#roll").show();
                $("#submitA").hide();
                $("#submitB").hide();
                $("#submitC").hide();
                $("#submitD").hide();
                $("#SCORE3").hide();
                winner(win);                   
            }                   

            if (data.substring(0,5) == "LOSER") { 
                DS_T = -1;
                DS_T2 = -1;
                var lose = e.data.substring(5);                    
                $("#roll").show();
                $("#submitA").hide();
                $("#submitB").hide();
                $("#submitC").hide();
                $("#submitD").hide();
                $("#SCORE3").hide();
                loser(lose);
            }

             if (data.substring(0,6) == "LOSER2") { 
                DS_T = -1;
                T4 = -1;
                var lose = e.data.substring(6);
                if (DS_player == DS_impossiblePlayer) {
                    DS_score += 2;
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

            else if (data.substring(0,5) == "LOGIN") {
                $("#output").html(e.data.substring(5) + " has joined.");
            }     
        };

// *************************************************************   Sign-in ***

        form.addEventListener("submit", function(e){
            e.preventDefault();
            f9 (input.value)
            input.value = "l";
            $("#output2").html("The game begins by clicking 'ROLL' ");
        });

        |]

main :: IO ()
main = do 
    chan <- atomically newBroadcastTChan
    warp 3000 $ App chan
