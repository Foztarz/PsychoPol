import random

# Set a seed
seed = random.randrange(1_000_000)
random.seed(seed)

print("Seed used:", seed)

# Generate 53 random values between 0 and 360
values = [random.randint(0, 360) for _ in range(78)]

print("Random values:")
for value in values:
    print(value)
