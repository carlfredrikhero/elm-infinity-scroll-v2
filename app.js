import { Elm } from "./src/Main.elm";

const app = Elm.Main.init();

/**
 * Listen to viewport closing in on the selector
 * , trigger the callback with the id of the entry that enters the viewport
 * @param string selector
 * @param function callback
 */
const sentinelObserver = function (selector, callback) {
  const elements = document.querySelectorAll(selector);
  const options = {
    rootMargin: "100% 0",
  };

  const observer = new IntersectionObserver((entries) => {
    const activeSentinels = entries
      .filter((entry) => entry.intersectionRatio > 0)
      .map((entry) => entry.target.id)
      .filter(unique)
      .forEach(callback);
  }, options);

  elements.forEach((el) => observer.observe(el));
};

sentinelObserver(".sentinel", app.ports.loadmore.send);

// OBSERVE CHANGES TO GRID CONTAINER

// Select the node that will be observed for mutations
const targetNode = document.querySelector(".grid");

// Create an observer instance linked to the callback function
const observer = new MutationObserver((mutationsList, observer) => {
  console.log("mutation observed");
  sentinelObserver(".sentinel", app.ports.loadmore.send);
});

// Start observing the target node for configured mutations
observer.observe(targetNode, { childList: true });

const unique = (value, index, self) => self.indexOf(value) === index;
