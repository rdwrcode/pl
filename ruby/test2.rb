# object state

class A
  def m1
    @foo = 0 # initialize internal state
  end

  def m2 x
    @foo += x # update the state
  end

  def foo
    @foo # get the state, nil if not initialized
  end
end

class B
  def initialize (f=0) # default
    @foo = f
  end
  def foo
    @foo
  end
  def m2 x
    @foo += x
  end
end

# class variables @@foo, shared by all instances of the same class
# class constants C::Foo
  
