import {Service, Inject} from "typedi";
import IMedicalConditionService from "./IServices/IMedicalConditionService";
import config from "../../config";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";

@Service()
export default class MedicalConditionService implements IMedicalConditionService {
    constructor(
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo
    ) {}

}
