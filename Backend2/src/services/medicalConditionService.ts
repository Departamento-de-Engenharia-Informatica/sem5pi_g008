import {Service, Inject} from "typedi";
import IMedicalConditionService from "./IServices/IMedicalConditionService";
import config from "../../config";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {Code} from "../domain/MedicalCondition/code";
import {Designation} from "../domain/MedicalCondition/designation";
import {Description} from "../domain/MedicalCondition/description";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";

@Service()
export default class MedicalConditionService implements IMedicalConditionService {
    constructor(
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo
    ) {
    }

    public async createMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<any> {

        const medicalConditionProps = {
            code: Code.create(medicalConditionDTO.code).getValue(),
            designation: Designation.create(medicalConditionDTO.designation).getValue(),
            description: Description.create(medicalConditionDTO.description).getValue(),
            symptomsList: medicalConditionDTO.symptomsList
        };

        const medicalCondition = MedicalCondition.create(medicalConditionProps).getValue();

        await this.medicalConditionRepo.save(medicalCondition, medicalConditionDTO.domainId);
    }

    public async getAllMedicalConditions(): Promise<IMedicalConditionDTO[]> {
        const medicalConditions = await this.medicalConditionRepo.getAll();
        return medicalConditions.map((medicalCondition) => MedicalConditionMap.toDTO(medicalCondition));
    }
    public async searchMedicalConditionsCode(query: any): Promise<IMedicalConditionDTO[]> {
        const medicalConditions = await this.medicalConditionRepo.searchCode(query);

        return medicalConditions.map((medicalCondition) => MedicalConditionMap.toDTO(medicalCondition));
    }
    public async searchMedicalConditionsDesignation(query: any): Promise<IMedicalConditionDTO[]> {
        const medicalConditions = await this.medicalConditionRepo.searchDesignation(query);

        return medicalConditions.map((medicalCondition) => MedicalConditionMap.toDTO(medicalCondition));
    }
}
