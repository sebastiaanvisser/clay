#!/bin/bash
runhaskell -i../src Main > new.css &&
  mv new.css style.css &&
  echo "done" ||
  echo "failed"
