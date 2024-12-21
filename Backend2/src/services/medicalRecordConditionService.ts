import {Inject, Service} from "typedi";
import config from "../../config";
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import IMedicalRecordConditionService from "./IServices/IMedicalRecordConditionService";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";

@Service()
export default class MedicalRecordConditionService implements IMedicalRecordConditionService {
    constructor(
        @Inject(config.repos.medicalRecordCondition.name) private medicalRecordConditionRepo: IMedicalRecordConditionRepo,
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo: IMedicalRecordRepo,
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo
    ) {
    }

    public async getMedicalRecordConditions(medicalRecordId: string): Promise<any> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordById(medicalRecordId);

        const medicalRecordConditionList = await this.medicalRecordConditionRepo.getMedicalRecordConditionsWithIds(medicalRecord.props._id.toString());

        const medicalRecordConditionDTOList = [];
        
        for (const medicalRecordCondition of medicalRecordConditionList) {
            
            const condition = await this.medicalConditionRepo.getMedicalConditionByBusinessId(medicalRecordCondition.conditionId);

            const medicalConditionDTO = MedicalRecordConditionMapper.toDTO(medicalRecordCondition, condition.designation, condition.id, medicalRecord.id);
        
            medicalRecordConditionDTOList.push(medicalConditionDTO);
        }
        
        return medicalRecordConditionDTOList;
    }

}