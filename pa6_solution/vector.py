from misc import Failure

class Vector(object):
    """ Vector class is a fixed length vector that implements a variety of
    functions """

    def __init__(self,args):
        """ this is the ctor for the Vector class, it takes in 
        takes in one arg and if the arg is a int or long it
        checks to make sure that the value is positive.
        if it is a list it uses that list as the values and 
        length of the vector. It raises an error on other inputs"""    
        if isinstance(args, int) or isinstance(args,long):
            if (args < 0):
                raise ValueError(" Must be a positive number")
            self.len = args
            self.values = [0.0] * args 
        elif isinstance(args, list):
            self.values = list(args)    
            self.len = len(args)
        else:
           raise TypeError("Parameter is not of correct type")

    def __repr__(self):
        """ repr is the string represention of the class, it returns
        Vector(contents of the list)"""
        return "Vector(" + repr(self.values) + ")"

    def __len__(self):
        """ len returns the length of the vector"""
        return len(self.values)

    def __iter__(self):
        """ iter returns an object that can iterate over the values of the
        vector. it uses yield to iterate"""
        for x in self.values:
            yield x

    def __add__(self, other):
       """ add is used to add the elements of a vector to a sequence
       to return another vector, zip is used to bind elemnents"""
       return Vector(([x + y for x, y in zip(list(self), list(other))])) 
 
    def __iadd__(self, other):
        """ iadd is used to implement the augmented method +=, the length
        of other needs to be the same length as self"""
        self.values = Vector(([x + y for x, y in zip(list(self), list(other))]))
        return self.values

    def __radd__(self, other):
        """ radd is used for when self =  the list and the second parameter
        other is the vector """
        return Vector(([x + y for x, y in zip(list(self), list(other))]))

    def dot (self, other):
       """ dot takes in a list or another vector and returns the dot product the
       arguements, it is the sum of the products of the components"""
       return sum([ x * y for x, y in zip(self, other)])

    def __getitem__ (self, x):
        """ getitem allows element accesss to the Vector, it gets the values of the
        item at x in self """
        return self.values[x]

    def __setitem__ (self, x, y): 
        """ setitem sets the values at location x with y, it also checks to make sure
        that the operation has not changed the length of the vector""" 
        temp = len(self.values)    
        self.values[x] = y
        if ( temp != len(self.values)):
            raise ValueError(" Can not change length of vector")

    def __eq__(self, other):
        """ eq checks for equality of two vectors, all the respective elements
        of the two vectors must equal each other for True to be retruned, if the
        other parameter is not of type vector false is returned """
        if not isinstance(other, Vector):
            return False
        bools =  ([ x == y for x, y in zip(self, other)])
        for x in bools:#check all the values of bool
            if x == False:
               return False
        return True

    def __ne__(self, other):
       """ ne is the opposite of eq so the return value is the opposite of eq"""
       return not self.__eq__(other)

    def __lt__(self, other):
        """ lt is less than, which is the opposite of greater than or equal,
        so the  return value is not ge"""
        return not self.__ge__(other)

    def __ge__(self, other):
        """ ge is greater than or equal, if the other is greater than true is 
        returned, and if the sorted elements are equal than true is returned"""
        if self.__gt__(other):
            return True
        selfSorted = sorted(self, reverse = True)
        otherSorted= sorted(other, reverse = True)
        if selfSorted.__eq__(otherSorted):
            return True
        return False

    def __gt__(self, other):
        """ gt is greater than, if the the two vectors are sorted and if the largest
        element is > then true is returned else the next value is checked"""
        if not isinstance( other, Vector):
            return (self > other)
        selfSorted = sorted(self, reverse = True)
        otherSorted= sorted(other, reverse = True)
        for x, y in zip(selfSorted, otherSorted):
           if x > y:
              return True
           elif x < y:
              return False
        return False

    def __le__(self, other):
        """ le is less than or equal which is the opposited of greater than, so the
        return value is the not of greater than"""
        return not self.__gt__(other)


