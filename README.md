# Testing-STMTree

An STMTree (will change the name soon) is a tree with TMVar nodes that can be modified when an event occurs.

The typical use case would be to:

    1) Create an STMTree (sTree)
    2) Modify a TMVar in the STMTree
    3) Notify that the STMTree has been modified

Issues are currently that there can only be a single thread that is notified