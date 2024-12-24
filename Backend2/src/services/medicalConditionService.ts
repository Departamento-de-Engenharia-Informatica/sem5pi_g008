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

  public async updateMedicalConditionDescription(id: string, newDescription:string): Promise<IMedicalConditionDTO> {
    const medicalCondition = await this.medicalConditionRepo.getByDomainId(Number.parseInt(id));
    medicalCondition.description = Description.create(newDescription).getValue();
    await this.medicalConditionRepo.updateUsingDomainId(medicalCondition,'description');
    return MedicalConditionMap.toDTO(medicalCondition);
  }

  public async updateMedicalConditionSymptoms(id: string,newSymptoms: string[]): Promise<IMedicalConditionDTO> {
    const medicalCondition = await this.medicalConditionRepo.getByDomainId(Number.parseInt(id));
    medicalCondition.updateSymptomsList(newSymptoms);
    await this.medicalConditionRepo.updateUsingDomainId(medicalCondition,'symptomsList');
    return MedicalConditionMap.toDTO(medicalCondition);
  }
}
