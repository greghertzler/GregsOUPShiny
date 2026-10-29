cliptext = "";
clipdeny = false;
tgglcnt = 0;

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
          if(barIdOUP == "tabROOUP") { plotId="#plot"+ROIdOUP; }
          else if(barIdOUP == "tabAOUP") { plotId="#plot"+AIdOUP; }
          else if(barIdOUP == "tabFDOUP") { plotId="#plot"+FDIdOUP; }
          else if(barIdOUP == "tabMLOUP") { plotId="#plot"+MLIdOUP; }
          else { plotId="#plot"+MCIdOUP; }
          $(plotId).click();
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
            else if(btnClass == "btn-toggle-title")
            {
              if(barIdOUP == "tabROOUP") { toggleId="tggl"+ROIdOUP; }
              else if(barIdOUP == "tabAOUP") { toggleId="tggl"+AIdOUP; }
              else if(barIdOUP == "tabFDOUP") { toggleId="tggl"+FDIdOUP; }
              else if(barIdOUP == "tabMLOUP") { toggleId="tggl"+MLIdOUP; }
              else { toggleId="tggl"+MCIdOUP; }
              tgglcnt = tgglcnt+1;
              Shiny.setInputValue(toggleId,tgglcnt);
            }
            else if(btnClass == "btn-undn-args")
            {
              if(barIdOUP == "tabROOUP") { undnId="#undn"+ROIdOUP; }
              else if(barIdOUP == "tabAOUP") { undnId="#undn"+AIdOUP; }
              else if(barIdOUP == "tabFDOUP") {undnId="#undn"+FDIdOUP; }
              else if(barIdOUP == "tabMLOUP") { undnId="#undn"+MLIdOUP; }
              else { undnId="#undn"+MCIdOUP; }
              $(undnId).click();
            }
            else if(btnClass == "btn-unup-args")
            {
              if(barIdOUP == "tabROOUP") { unupId="#unup"+ROIdOUP; }
              else if(barIdOUP == "tabAOUP") { unupId="#unup"+AIdOUP; }
              else if(barIdOUP == "tabFDOUP") { unupId="#unup"+FDIdOUP; }
              else if(barIdOUP == "tabMLOUP") { unupId="#unup"+MLIdOUP; }
              else { unupId="#unup"+MCIdOUP; }
              $(unupId).click();
            }
            else if(btnClass == "btn-left-plot")
            {
              if(barIdOUP == "tabROOUP") { prevId="#left"+ROIdOUP; }
              else if(barIdOUP == "tabAOUP") { prevId="#left"+AIdOUP; }
              else if(barIdOUP == "tabFDOUP") {prevId="#left"+FDIdOUP; }
              else if(barIdOUP == "tabMLOUP") { prevId="#left"+MLIdOUP; }
              else { prevId="#left"+MCIdOUP; }
              $(prevId).click();
            }
            else if(btnClass == "btn-rght-plot")
            {
              if(barIdOUP == "tabROOUP") { nextId="#rght"+ROIdOUP; }
              else if(barIdOUP == "tabAOUP") { nextId="#rght"+AIdOUP; }
              else if(barIdOUP == "tabFDOUP") { nextId="#rght"+FDIdOUP; }
              else if(barIdOUP == "tabMLOUP") { nextId="#rght"+MLIdOUP; }
              else { nextId="#rght"+MCIdOUP; }
              $(nextId).click();
            }
            else if(btnClass == "btn-othr-plot")
            {
              if(barIdOUP == "tabROOUP") { othrId="#othr"+ROIdOUP; }
              else if(barIdOUP == "tabAOUP") { othrId="#othr"+AIdOUP; }
              else if(barIdOUP == "tabFDOUP") { othrId="#othr"+FDIdOUP; }
              else if(barIdOUP == "tabMLOUP") { othrId="#othr"+MLIdOUP; }
              else { othrId="#othr"+MCIdOUP; }
              $(othrId).click();
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
