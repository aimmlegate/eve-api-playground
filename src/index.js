import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";
import marketGroups from "./api-dumps/marketGroups.json";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: marketGroups
});

registerServiceWorker();
