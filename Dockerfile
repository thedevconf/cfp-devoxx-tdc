FROM globalcode/scala-sbt
RUN mkdir /cfp-devoxx-dev
WORKDIR /cfp-devoxx-dev
EXPOSE 9000
COPY project/build.properties project/plugins.sbt project/
COPY build.sbt .
RUN sbt update
COPY . .
CMD ./run.sh