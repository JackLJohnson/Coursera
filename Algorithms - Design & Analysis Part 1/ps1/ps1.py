

import timing

# Global Variable to keep track of number of inversions
_numInversions = 0 

def mergeSort(nums):
  """
    Sorts an array using merge sort, which takes O(nlog(n)) time
  """
  if len(nums) == 1:
    return [nums[0]]
  else:
    middle = len(nums)/2
    left = nums[:middle]
    right = nums[middle:]
    return merge(mergeSort(left), mergeSort(right))
  
def merge(left, right):
  """
    Merges two sorted arrays in O(n) time, 
    while also counting the number of inversions in the array(s)
  """
  global _numInversions
  # Check for corner cases of either array being empty
  if right is None : return left
  elif left is None: return right
  
  mergedList = []
  leftlen    = len(left)
  rightlen   = len(right)

  # Cursors for iterating through left and right half, respectively
  lc = rc = 0

  for i in xrange(leftlen + rightlen):
    # If either half is empty, 
    # then simple concatenate the remaining elements of the other array to result
    if lc >= leftlen:
      mergedList.extend(right[rc:])
      break
    elif rc >= rightlen:
      mergedList.extend(left[lc:])
      break
    # Check which element is smaller (left or right half's next element), 
    # then append smaller element to result
    elif left[lc] <= right[rc]:
      mergedList.append(left[lc])
      lc += 1
    else:
      _numInversions += leftlen - lc
      mergedList.append(right[rc])
      rc += 1
  return mergedList

def reverseList(l):
  b, e = 0, len(l)-1
  for i in xrange(len(l)/2):
    l[b], l[e] = l[e], l[b]
    e -= 1
    b += 1
  return l

if __name__ == "__main__":
  nums = []
  fi = open('IntegerArray.txt', 'r')
  
  for line in fi: 
    nums.append(int(line))

  sortedNums = mergeSort(nums)
  print _numInversions

  test = [1,3,4,5,7,8,9]
  print test
  print reverseList(test)

