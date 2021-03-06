﻿# Manchester F# User Group Stop Loss Kata

I thought it might be fun to try [Greg Young's Stop Loss Kata](https://gist.github.com/gregoryyoung/1500720)
using the Given/When/Then style of testing events as in the [SAFE ConfPlanner](https://github.com/SAFE-Stack/SAFE-ConfPlanner)

For reference:


Greg's Stop Loss Kata

Testing is very hard when time is involved ...

A trailing stop loss is a term used in financial trading. For a very in depth explanation you can read here http://www.investopedia.com/articles/trading/03/080603.asp and http://en.wikipedia.org/wiki/Order_(exchange)#Stop_orders

However we do not need a huge amount of background in order to do the kata as we are going to limit the problem a bit.

The general idea is that when you buy into a stock at a price say $10. You want it to automatically get sold if the stock goes below $9 (-$1). If we use the term "trailing" that means that id the price goes up to $11 then the sell point becomes $10.

The kata is to create something that implements a trailing stop loss and to do it with TDD. To make matters more fun a price point should only move up if its held for more than 15 seconds and the stop loss should only be triggered if the price point is held for more than 30 seconds.

You will receive a "PriceChanged" message every time the price changes. Just implement it as a method that receives that message (assume later you will hook it up into something that provides that)

It will be fun to write the tests no?