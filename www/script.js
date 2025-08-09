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
// wells fresh and stale
  function wellfresh(){
          wells = document.getElementsByClassName("well wellPlotOUP");
          for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor=fresh; };
          wells = document.getElementsByClassName("well wellTableOUP");
          for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor=fresh; };
        }
  function wellstale(){
          wells = document.getElementsByClassName("well wellPlotOUP");
          for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor=stale; };
          wells = document.getElementsByClassName("well wellTableOUP");
          for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor=stale; };
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
          };
        }
// combobox and file input change
  $(document).on("change",function(){
          if(lastEvent == "click")
          {
            activeEl=document.activeElement;
            if(activeEl.tagName == "INPUT")
            {
              if(activeEl.attributes[5].value == "combobox" | activeEl.attributes[3].value == "file") { wellstale(); };
            };
          };
        });
// mouse fresh
  $(document).on("click",function(){
          activeEl=document.activeElement;
          if(activeEl.tagName == "BUTTON")
          {
            btnClass=activeEl.attributes[0].value;
            if(btnClass.includes("btn-success")) { wellfresh(); }
          }
          else if(activeEl.tagName == "A") { wellfresh(); };
          lastEvent = "click";
        });
// keyboard fresh or stale
  $(document).on("keyup",function(e){
          if(e.key == "Enter")
          {
            plotit();
            wellfresh();
          }
          else if(e.key == "1" | e.key == "2" | e.key == "3" | e.key == "4" | e.key == "5" | e.key == "6" | e.key == "7" | e.key == "8" | e.key == "9" | e.key == "0" | e.key == "-" | e.key == "+" | e.key == "." | e.key == "Delete" | e.key == "Backspace")
          {
            activeEl=document.activeElement;
            if(activeEl.tagName == "INPUT") { wellstale(); };
          };
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
              wellfresh();
            };
          };
        });
});
