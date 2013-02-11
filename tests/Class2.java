package tests;

/**
 * This is a multi-line
 * comment.
 */
public class Class2 {
	public Class2() {
	}

	enum MyEnum { VAL1, VAL2 }

	public static class MyComparator implements Comparator<String> {
		@Override
		public int compare(String s1, String s2) {
			return 0;
		}
	}
	
}
