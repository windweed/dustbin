# std::map

## struct as key

需要认真设计 `operator<` 函数

```cpp
struct Key_STInfo {
	uint8_t matchMethod;
	uint32_t protoId;

	Key_STInfo() {
		memset(this, 0, sizeof(struct Key_STInfo));
	}
	
	Key_STInfo(uint8_t method, uint32_t id) {
		matchMethod = method;
		protoId = id;
	}

	bool operator<(const struct Key_STInfo& a) const {
		if (matchMethod < a.matchMethod) {
			return true;
		} else if (matchMethod == a.matchMethod) {
			return protoId < a.protoId;
		} else {
			return false;
		}
	}
};
```


