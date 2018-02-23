module venus.exception;
import venus.context;

class CompileException : Exception {

	Location loc;
	string error;
	this(Location loc, string error)
	{
		super(error);
		this.loc = loc;
		this.error = error;
	}
}

