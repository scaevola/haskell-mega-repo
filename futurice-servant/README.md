# futurice-servant

Collection of servant utilities for servant

## CloudWatch

Services build on `futurice-servant` log (text and metrics) to cloudwatch
if `CLOUDWATCH_ACCESSKEY` and `CLOUDWATCH_SECRETKEY` environment variables are set.

Using `CLOUDWATCH_SUFFIX` variable, one can alter the name of metrics and cloudwatch log stream name.

*Note:* CloudWatch Logs Stream have to be created from the console.
TODO: Should they be autocreated?

Used actions (for policy):

```javascript
[
  "cloudwatch:PutMetricData",
  "cloudwatch:ListMetrics",
  "logs:PutLogEvents",
  "logs:DescribeLogStreams"
]
```
