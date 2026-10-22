cliptext = "";
clipdeny = false;

Shiny.addCustomMessageHandler('sendToShiny', function(txt) {
          cliptext=txt;
        });

$(document).ready(function() {
  lastEvent = "";
// dark mode
  fresh = "#223344";
  stale = "#334455";
  dm = "dark";
// bootstrap tabs
  barIdOUP="tabROOUP";
          $("#navBar").on("shown.bs.tab",function(event){
            barIdOUP = event.target.attributes[3].value;
          });
  ROIdOUP="RODataOUP";
          $("#navROOUP").on("shown.bs.tab",function(event){
            ROIdOUP = event.target.attributes[3].value;
          });
  AIdOUP="ADriftOUP";
          $("#navAOUP").on("shown.bs.tab",function(event){
            AIdOUP = event.target.attributes[3].value;
          });
  FDIdOUP="FDDriftOUP";
          $("#navFDOUP").on("shown.bs.tab",function(event){
            FDIdOUP = event.target.attributes[3].value;
          });
  MLIdOUP="MLDataOUP";
          $("#navMLOUP").on("shown.bs.tab",function(event){
            MLIdOUP = event.target.attributes[3].value;
          });
  MCIdOUP="MCForwardOUP";
          $("#navMCOUP").on("shown.bs.tab",function(event){
            MCIdOUP = event.target.attributes[3].value;
          });
// cards fresh and stale
  function cardfresh(){
          cards = document.getElementsByClassName("card cardPlotOUP");
          for(i=0; i<cards.length; i++) { cards[i].style.backgroundColor=fresh; }
          cards = document.getElementsByClassName("card cardTableOUP");
          for(i=0; i<cards.length; i++) { cards[i].style.backgroundColor=fresh; }
        }
  function cardstale(){
          cards = document.getElementsByClassName("card cardPlotOUP");
          for(i=0; i<cards.length; i++) { cards[i].style.backgroundColor=stale; }
          cards = document.getElementsByClassName("card cardTableOUP");
          for(i=0; i<cards.length; i++) { cards[i].style.backgroundColor=stale; }
        }
// send plot click
  function plotit(){
          if(barIdOUP == "tabROOUP")
          {
            plotId="#plot"+ROIdOUP;
            $(plotId).click();
          }
          else if(barIdOUP == "tabAOUP")
          {
            plotId="#plot"+AIdOUP;
            $(plotId).click();
          }
          else if(barIdOUP == "tabFDOUP")
          {
            plotId="#plot"+FDIdOUP;
            $(plotId).click();
          }
          else if(barIdOUP == "tabMLOUP")
          {
            plotId="#plot"+MLIdOUP;
            $(plotId).click();
          }
          else if(barIdOUP == "tabMCOUP")
          {
            plotId="#plot"+MCIdOUP;
            $(plotId).click();
          }
        }
// combobox and file input change
  $(document).on("change",function(){
          if(lastEvent == "click")
          {
            activeEl=document.activeElement;
            if(activeEl.tagName == "INPUT")
            {
              if(activeEl.attributes[5].value == "combobox" | activeEl.attributes[3].value == "file") { cardstale(); }
            }
          }
        });
// mouse fresh and copy data and arguments
  $(document).on("click",function(){
          activeEl=document.activeElement;
          if(activeEl.tagName == "BUTTON")
          {
            btnClass=activeEl.attributes[0].value;
            if(btnClass.includes("btn-success")) { cardfresh(); }
            else if(btnClass.includes("btn-copy"))
            {
              if(clipdeny) {
                activeEl.classList.toggle('disabled');
                Shiny.setInputValue("clipboardDeny",window.location.host,{ priority: "event" });
              }
              else
              {
                navigator.clipboard.writeText(cliptext).catch(function(err) {
                  Shiny.setInputValue("clipboardDeny",window.location.host,{ priority: "event" });
                  clipdeny = true;
                });
              }
            }
          }
          else if(activeEl.tagName == "A") { cardfresh(); }
          lastEvent = "click";
        });
// keyboard fresh or stale
  $(document).on("keyup",function(e){
          if(e.key == "Enter")
          {
            plotit();
            cardfresh();
          }
          else if(e.key == "1" | e.key == "2" | e.key == "3" | e.key == "4" | e.key == "5" | e.key == "6" | e.key == "7" | e.key == "8" | e.key == "9" | e.key == "0" | e.key == "-" | e.key == "+" | e.key == "." | e.key == "Delete" | e.key == "Backspace")
          {
            activeEl=document.activeElement;
            if(activeEl.tagName == "INPUT") { cardstale(); }
          }
          lastEvent = "keyup";
        });
// transition for darkmodeswitch which does not share its toys
  $(document).on("transitionrun",function(){
          activeEl=document.activeElement;
          if(activeEl.id == "darkmodeswitch")
          {
            if(activeEl.mode != dm)
            {
              if(dm == "dark")
              {
                fresh = "#ddeeff";
                stale = "#ccddee";
                dm = "light";
              }
              else
              {
                fresh = "#223344";
                stale = "#334455";
                dm = "dark";
              }
              plotit();
              cardfresh();
            }
          }
        });
// alert for server disconnection
  $(document).on('shiny:disconnected', function() {
          alert('Connection lost!\n\n  Causes may be: idle timeout, network dropout or exceeding the server limits.\n\n  If your problem exceeds the server limits, you may wish to install GregsOUPR6 on your computer.  See the Calculations document on the Help menu for details.');
        });
});
