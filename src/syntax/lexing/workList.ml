module LinkedNode = struct
	type 'a t = {
		mutable data:'a;
		mutable next:'a t;
	}

	let insert node data =
		let new_node = {
			data = data;
			next = node.next;
		} in
		node.next <- new_node;
		new_node

	let advance_by node f i =
		let rec loop node acc k =
			if k = i then node,List.rev acc else loop node.next (f node :: acc) (k + 1)
		in
		loop node [] 0

	let to_list f t =
		let rec loop t =
			if t == t.next then [] else f t :: loop t.next
		in
		loop t

	let print f sep t =
		let rec loop t =
			(f t.data) ^ (if t == t.next then "" else sep ^ loop t.next)
		in
		loop t
end

open LinkedNode

type 'a t = {
	first:'a LinkedNode.t;
	mutable insert:'a LinkedNode.t;
	mutable work:'a LinkedNode.t;
}

let create neutral =
	let rec node_end = {
		data = neutral;
		next = node_end;
	} in
	let rec node_start = {
		data = neutral;
		next = node_end;
	} in
	{
		first = node_start;
		insert = node_start;
		work = node_start;
	}

let push t data =
	let node = insert t.insert data in
	t.insert <- node

let insert t data =
	let node = insert t.work data in
	if t.work == t.insert then t.insert <- node

let advance t =
	let node = t.work.next in
	let data = fst node.data in
	t.work <- node;
	data

let advance_by t f i =
	let node,l = LinkedNode.advance_by t.work f i in
	t.work <- node;
	l

let current t =
	t.work.next