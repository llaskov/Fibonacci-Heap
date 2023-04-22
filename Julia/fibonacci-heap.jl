# Fibonacci heap minimum priority queue data structure

# Fibonacci heap node
mutable struct FHeapNode{T}

    # data fields
    key::T                                  # key that is stored in the node
    mark::Bool                              # whether the node has lost child since it was made a child
    degr::Int                               # degree of the node is the number of children nodes
    prnt::Union{FHeapNode{T}, Nothing}      # reference to the parent node
    chld::Union{FHeapNode{T}, Nothing}      # reference to a child node
    prev::Union{FHeapNode{T}, Nothing}      # reference to the previous sibling
    next::Union{FHeapNode{T}, Nothing}      # reference to the next sibling

    # constructor that sets the data key field
    function FHeapNode{T}(key::T) where T
        node = new{T}(key)
        node.mark = false
        node.degr = 0
        node.prnt = nothing
        node.chld = nothing
        node.prev = node
        node.next = node

        return node;
    end
end

# number of children of a node
degree(node::FHeapNode) = node.degr

# whether the child list is empty
isemptychild(node::FHeapNode) = node.chld == nothing

# attach a node to a circular doubly-linked list defined by a node
function attach!(head::Union{FHeapNode{T}, Nothing}, node::FHeapNode{T}) where T
    if head == nothing
        # this is the first node in the list
        head = node
        head.next = head                    # keep list circular
        head.prev = head
    elseif head.next == head
        # the list node is the only node in the list
        head.next = node                    # attach the new node as second
        head.prev = node                    # keep the list circular
        node.prev = head                    # attach the previous link of the new node
        node.next = head                    # keep the list circular
    else
        # there is more than one node in the list
        aftr = head.next                    # the node after the head node
        head.next = node                    # attach the new node after the list node
        node.prev = head                    # the previous of the new node becomes the head
        node.next = aftr                    # connect new node to next
        aftr.prev = node                    # connect the node after to the new node
    end

    return head
end

# detach a node to a circular doubly-linked list but preserve the links of the removed node
function detach!(head::Union{FHeapNode{T}, Nothing}, node::FHeapNode{T}) where T
    head == nothing && throw(ArgumentError("Remove a node from an empty linked list."))

    if head.next == head
        node != head && 
            throw(DomainError(node.key, "Detach the last node, but it is not equal to the head."))
        # detach the last node from the list
        head = nothing
    else
        if node == head
            # the node to remove is the node that holds the list
            head = head.next
        end
        node.prev.next = node.next
        node.next.prev = node.prev
    end

    return head
end

# add a node to the child list of a node
function addchild!(prnt::FHeapNode{T}, chld::FHeapNode{T}) where T
    chld.prnt = prnt                        # set the parent of the child node
    chld.mark = false                       # reset the mark field of the child node
    # attach child node to the child list of the parent node
    prnt.chld = attach!(prnt.chld, chld) 
    prnt.degr += 1                          # increment degree of the parent node
end

# remove a node from the child list and return it as a result
function removechild!(prnt::FHeapNode{T}) where T
    result = prnt.chld
    prnt.chld = detach!(prnt.chld, result)
    result.prnt = nothing                   # reset the link to the parent node
    prnt.degr -= 1                          # update degree of the parent node

    return result
end

# print a node data structure
function Base.print(node::FHeapNode{T}) where T
    println("[Key: ", node.key, "]")
    println("Mark: ", node.mark)
    println("Degree: ", degree(node))
    print("Child list: ")
    node.chld != nothing ? lstprint(node.chld) : println("empty")
end

# get an array of nodes from a list given its head
function getarray(head::FHeapNode{T}) where T
    result = FHeapNode{T}[]
    more = true
    curr = head
    while more
        push!(result, curr)
        curr = curr.next
        if curr == head
            more = false
        end
    end

    return result
end

# connect two circular doubly-linked lists, where each list is given using a single node reference
function connect!(fhead::Union{FHeapNode{T}, Nothing}, shead::Union{FHeapNode{T}, Nothing}) where T
    (fhead == nothing || shead == nothing) && 
        throw(ArgumentError("Connect an empty list."))
    # both lists are not empty
    ftail = fhead.next                  # the node after the head becomes tail in first list
    stail = shead.next                  # the node after the head becomes tail in second list
    fhead.next = stail
    stail.prev = fhead
    shead.next = ftail
    ftail.prev = shead
    return fhead
end

# print the constants of a circular doubly-linked list
function lstprint(head::FHeapNode{T}) where T
    curr = head
    more = true
    while more
        print(curr.key, " ")
        curr = curr.next
        if curr == head
            more = false
        end
    end
    println()
end

# Fibonacci heap priority queue
mutable struct FibonacciHeap{T}

    # data fields
    min::Union{FHeapNode{T}, Nothing}       # reference to the root of the tree containing the minimum
    numb::Int                               # number of elements currently stored in the Fibonacci heap

    # constructor without parameters
    function FibonacciHeap{T}() where T
        fheap = new{T}()
        fheap.min = nothing
        fheap.numb = 0

        return fheap
    end
end

# return the size of the Fibonacci heap
Base.size(fheap::FibonacciHeap) = fheap.numb

# check whether the Fibonacci heap is empty
Base.isempty(fheap::FibonacciHeap) = fheap.min == nothing

# get the key of the minimum node in the Fibonacci heap
minimum(fheap::FibonacciHeap) = !isempty(fheap) ? fheap.min.key : nothing

# calculate upper bound on the maximum degree
maxdeg(fheap::FibonacciHeap) = round(Int, floor(log(Base.MathConstants.golden, fheap.numb)))

# attach a node to the root list
function attachtoroot!(fheap::FibonacciHeap{T}, node::FHeapNode{T}) where T       
    # attach the new node into the root list
    fheap.min = attach!(fheap.min, node)   
end

# insert a new key into the Fibonacci heap
function insert!(fheap::FibonacciHeap{T}, key::T) where T    
    node = FHeapNode{T}(key)                # create the new node
    attachtoroot!(fheap, node)              # attach the new node to the root list
    if node.key < fheap.min.key
        fheap.min = node                    # update the pointer to the minimum if needed
    end
    fheap.numb += 1                         # update number of elements in the heap
end

# remove a node from the root list and add it as a child of another node
function link!(fheap::FibonacciHeap{T}, y::FHeapNode{T}, x::FHeapNode{T}) where T
     # remove y from the root list
     fheap.min = detach!(fheap.min, y)     
     # add y as a child of x
     addchild!(x, y)
end

# consolidate the Fibonacci heap
function consolidate!(fheap::FibonacciHeap{T}) where T
    # upper bound on the maximum degree plus one because indexing starts from 1
    size = maxdeg(fheap) + 1

    # look-up table of pointers to the roots according to their degree
    tbl = Array{Union{Nothing, FHeapNode{T}}}(nothing, size)

    # for each node in the root list of the Fibonacci heap
    rootlist = getarray(fheap.min)
    for x in rootlist
        d = degree(x) + 1
        while tbl[d] != nothing
            y = tbl[d]
            if x.key > y.key
                x, y = y, x
            end
            link!(fheap, y, x)
            tbl[d] = nothing
            d += 1
        end
        tbl[d] = x
    end

    # find the new minimum node
    for node in tbl
        if node != nothing && node.key < fheap.min.key
            fheap.min = node
        end
    end
end

# extract the minimum node and consolidate the Fibonacci heap
function extractmin!(fheap::FibonacciHeap{T}) where T
    min = fheap.min
    if min != nothing
        # add children nodes of the minimum to the root list
        while !isemptychild(min)
            chld = removechild!(min)        # extract a child node
            attachtoroot!(fheap, chld)      # attach the child node to the root list
        end
        # remove the minimum from the root list
        fheap.min = detach!(fheap.min, min)
        if min != min.next
            # min is not the last node in the heap
            consolidate!(fheap)             # consolidate the Fibonacci heap
        end
        fheap.numb -= 1
    end
    return min
end

# uniting two Fibonacci heaps which destroys the two input heaps
function union!(fheap_frst::FibonacciHeap{T}, fheap_scnd::FibonacciHeap{T}) where T

    # create the resulting Fibonacci heap
    result = FibonacciHeap{T}()

    # concatenate two root lists
    result.min = connect!(fheap_frst.min, fheap_scnd.min)

    # update the minimum if needed
    if (isempty(fheap_frst) || (!isempty(fheap_scnd) && (fheap_scnd.min.key < fheap_frst.min.key)))
        result.min = fheap_scnd.min
    end

    # set number of nodes in the root list
    result.numb = fheap_frst.numb + fheap_scnd.numb

    return result
end

# cut the link between the parent and the child node, making the child a root
function cut!(fheap::FibonacciHeap{T}, node::FHeapNode{T}, prnt::FHeapNode{T}) where T
    prnt.chld = detach!(prnt.chld, node)
    prnt.degr -= 1                          # update degree of the parent node
    node.prnt = nothing                     # reset the link to the parent node
    node.mark = false                       # unmark the detached child node
    attachtoroot!(fheap, node)              # attach the detached child node to the root list
end

# cascading cut operation
function cascadingcut!(fheap::FibonacciHeap{T}, node::FHeapNode{T}) where T
    prnt = node.prnt
    if prnt != nothing
        if node.mark == false
            node.mark = true
        else
            cut!(fheap, node, prnt)
            cascadingcut!(fheap, prnt)
        end
    end
end

# decrease the key of a given node
function decreasekey!(fheap::FibonacciHeap{T}, node::FHeapNode{T}, key::T) where T
    (key > node.key) && throw(ArgumentError("The new key is grater than the current."))
    node.key = key
    prnt = node.prnt
    if prnt != nothing && node.key < prnt.key
        cut!(fheap, node, prnt)             # cut operation
        cascadingcut!(fheap, prnt)          # cascading cut operation
    end
    if node.key < fheap.min.key
        fheap.min = node
    end
end

# delete a node
function delete!(fheap::FibonacciHeap{T}, node::FHeapNode{T}) where T
    min = minimum(fheap)                    # find the current minimum
    min -= 1                                # decrease the current minimum
    decreasekey!(fheap, node, min)          # make the key of the node to delete the minimum
    extractmin!(fheap)                      # extract the minimum from the Fibonacci heap
end

# print a Fibonacci heap data structure
function Base.print(fheap::FibonacciHeap{T}) where T
    println("Size of the heap: ", size(fheap))
    println("The heap is empty: ", isempty(fheap))
    println("Minimum: ", minimum(fheap))
    if !isempty(fheap)
        print("Root list: ")
        lstprint(fheap.min)
    end
end

# Test data structures
const SIZE = 256

fheap = FibonacciHeap{Int}()
for i = 1:SIZE
    insert!(fheap, rand(10:99))
end
print(fheap)

node = extractmin!(fheap)
println("Extracted minimum: ", node.key)

node = fheap.min.next
println("Node key: ", node.key)
key = node.key - 10
println("New key: ", key)
decreasekey!(fheap, node, key)
print(fheap)

node = fheap.min.next
println("Node key: ", node.key)
key = node.key - 10
println("New key: ", key)
decreasekey!(fheap, node, key)
print(fheap)

node = fheap.min.next
println("Will delete node key: ", node.key)
delete!(fheap, node)
print(fheap)
