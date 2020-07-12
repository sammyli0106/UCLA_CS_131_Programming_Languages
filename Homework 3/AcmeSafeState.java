/* This is a new class that is safe 
without the using the keyword synchronized.
The goal is to ahcieve better performance than synnchronized state 
while being safe at the same time */

// Find out which function is safe to use atomically

import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.atomic.AtomicLongArray;

// Use atomic array 
class AcmeSafeState implements State {

	// Create a atomic long array 
	// Double check this one 
    private AtomicLongArray​ value;
    private ReentrantLock lock = new ReentrantLock();

    // Double check this one 
    AcmeSafeState(int length) { value = new AtomicLongArray​(length); }

    // Might need to modify the call of length method
    public int size() { return value.length(); }

    // We need to convert the atomic long array into long array and return it as output
    public long[] current() {
    	// create the long array which return as output
    	long [] output = new long[value.length()];

    	// A loop that copy the content from atomic array to long array 
    	for (int i = 0; i < output.length; i++)
    	{
    		output[i] = (long) value.get(i);
    	}

    	return output;
    }

    public void swap(int i, int j) {

    // Grab the lock here
    lock.lock();

    value.decrementAndGet(i);
    value.incrementAndGet(j);

	// Release the lock here
	lock.unlock();

    }
}
