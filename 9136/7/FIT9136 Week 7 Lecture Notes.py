#https://medium.com/@stephenagrice/how-to-implement-a-binary-search-tree-in-python-e1cdba29c533
class Node(object):
	def __init__(self, d, left = None, right = None):
		self.data = d
		self.left = left
		self.right = right
	
	def insert(self, d):
		if self.data == d:
			return False
		elif d < self.data:
			if self.left:
				return self.left.insert(d)
			else:
				self.left = Node(d)
				return True
		else:
			if self.right:
				return self.right.insert(d)
			else:
				self.right = Node(d)
				return True
	
	def find(self, d):
		if self.data == d:
			return True
		elif d < self.data and self.left:
			return self.left.find(d)
		elif d > self.data and self.right:
			return self.right.find(d)
		return False
	
	def traversal(self, l):
		'''traversal all nodes by the preorder'''
		l.append(self.data)
		if self.left:
			self.left.traversal(l)
		if self.right:
			self.right.traversal(l)
		return l

class BST(object):
	
	def __init__(self):
		self.root = None
	
	def insert(self, d):
		'''# return True if successfully inserted, false if exists'''
		if self.root:
			return self.root.insert(d)
		else:
			self.root = Node(d)
			return True

	def find(self, d):
		'''	# return True if d is found in tree, false otherwise'''
		if self.root:
			return self.root.find(d)
		else:
			return False

	
	def __str__(self):
		# return list of data elements resulting from preorder tree traversal
		if self.root:
			return ",".join(map(str, self.root.traversal([])))
		else:
			return "the tree is empty"


if __name__ == "__main__":
	test_tree = BST()   #initialize the binary search tree
	#add elements to the tree
	items = [3,5,4,7,6,1]
	for item in items:
		test_tree.insert(item)

	print(test_tree)   #tree traversal 
	print(test_tree.find(10))  #tree search