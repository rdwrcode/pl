class A
  def m1
    34
  end
  def m2 (x, y)
    z = 7
    if x > y
      false
    else
      x + y * z
    end
  end
end

a = A.new
a.m1
a.m2(2, 3)
a.m2(3, 4)

class B
  def m1
    4
  end
  def m2 x
    x.abs * 2 + self.m1 # call m1 of this object
  end
end

b = B.new
b.m1
b.m2(3)

class C
  def m1
    print "hi "
    self # return the object itself
  end
  def m2
    print "bye "
    self
  end
  def m3
    print "\n"
    self
  end
end

c = C.new
c.m1.m2.m3
    
