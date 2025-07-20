  lastEvent = "";
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
// combobox and file input change
  $(document).on("change",function(){
          if(lastEvent == "click")
          {
            activeEl=document.activeElement;
            if(activeEl.tagName == "INPUT")
            {
              if(activeEl.attributes[5].value == "combobox" | activeEl.attributes[3].value == "file")
              {
                wells = document.getElementsByClassName("well wellPlotOUP");
                for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(192,192,192)"; };
                wells = document.getElementsByClassName("well wellTableOUP");
                for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(192,192,192)"; };
              };
            };
          };
        });
// mouse fresh
  $(document).on("click",function(){
          activeEl=document.activeElement;
          if(activeEl.tagName == "BUTTON")
          {
            btnClass=activeEl.attributes[0].value;
            if(btnClass.includes("btn-success"))
            {
              wells = document.getElementsByClassName("well wellPlotOUP");
              for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(245,245,245)"; };
              wells = document.getElementsByClassName("well wellTableOUP");
              for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(245,245,245)"; };
            };
          }
          else if(activeEl.tagName == "A")
          {
            wells = document.getElementsByClassName("well wellPlotOUP");
            for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(245,245,245)"; };
            wells = document.getElementsByClassName("well wellTableOUP");
            for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(245,245,245)"; };
          };
          lastEvent = "click";
        });
// keyboard fresh or stale
  $(document).on("keyup",function(e){
          if(e.key == "Enter")
          {
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
            wells = document.getElementsByClassName("well wellPlotOUP");
            for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(245,245,245)"; };
            wells = document.getElementsByClassName("well wellTableOUP");
            for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(245,245,245)"; };
          }
          else if(e.key == "1" | e.key == "2" | e.key == "3" | e.key == "4" | e.key == "5" | e.key == "6" | e.key == "7" | e.key == "8" | e.key == "9" | e.key == "0" | e.key == "-" | e.key == "+" | e.key == "." | e.key == "Delete" | e.key == "Backspace")
          {
            activeEl=document.activeElement;
            if(activeEl.tagName == "INPUT")
            {
              numId = activeEl.attributes[0].value;
              if(numId.includes("beg") | numId.includes("end"))
              {
                wells = document.getElementsByClassName("well wellPlotOUP");
                for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(192,192,192)"; };
              }
              else
              {
                wells = document.getElementsByClassName("well wellPlotOUP");
                for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(192,192,192)"; };
                wells = document.getElementsByClassName("well wellTableOUP");
                for(i=0; i<wells.length; i++) { wells[i].style.backgroundColor="rgb(192,192,192)"; };
              };
            };
          };
          lastEvent = "keyup";
        });
