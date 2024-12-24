import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import IMedicalRecordDTO from "../dto/IMedicalRecordDTO";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import IMedicalRecordAllergyRepo from "./IRepos/IMedicalRecordAllergyRepo";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";


@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo,
        @Inject(config.repos.medicalRecordAllergy.name) private medicalRecordAllergyRepo: IMedicalRecordAllergyRepo,
        @Inject(config.repos.medicalCondition.name) private medicalRecordConditionRepo: IMedicalRecordConditionRepo,
        @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo
    ) {}
       

    public async createMedicalRecord(medicalRecordId:string): Promise<void> {
      const medicalProps = {};
      const medicalRecord = MedicalRecord.create(medicalProps);
      await this.medicalRecordRepo.save(medicalRecord.getValue(), medicalRecordId);
    }
    public async updateMedicalConditions(medicalRecordId: string, conditions: IMedicalRecordConditionDTO[]): Promise<void> {
        console.log("Updating medical conditions for medical record with ID SERVICE:", medicalRecordId);
        
        // Busca o registro médico pelo ID
        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordById(medicalRecordId);
        if (!medicalRecord) {
            throw new Error(`Medical record with ID ${medicalRecordId} not found`);
        }
        // Inicializa o array de condições atualizadas
        const updatedConditions: MedicalRecordCondition[] = [];
        console.log('Passou1');
        // Processa cada condição do DTO
        for (const conditionDTO of conditions) {
            // Busca a condição médica no repositório usando o ID
            console.log('IDIDID',(conditionDTO.domainId.toString()));
            const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionById(conditionDTO.domainId.toString());
            console.log('Passou2');
            if (!medicalRecordCondition) {
                throw new Error(`Medical condition with ID ${conditionDTO.domainId} not found`);
            } 
 
             // Converte a condição médica para o domínio
            const conditionDomain = MedicalRecordConditionMapper.toDomain(medicalRecordCondition);
 
            // Adiciona ao array de condições atualizadas
            updatedConditions.push(conditionDomain);
        } 
 
        // Atualiza as condições no registro médico
        medicalRecord.medicalRecordConditions = updatedConditions;
        // Salva o registro médico atualizado
        await this.medicalRecordRepo.save(medicalRecord, medicalRecordId); 
    }
  
    public async getAllergies(medicalRecordId:string): Promise<IMedicalRecordAllergyDTO[]> {


      const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);
      const medicalRecordPrivateId = medicalRecord.props._id.toString();

      const medicalRecordAllergies = await this.medicalRecordAllergyRepo.getByMedicalId(medicalRecordPrivateId);

      console.log(medicalRecordAllergies);

      let aux : IMedicalRecordAllergyDTO[] = [];

      for(let i = 0; i < medicalRecordAllergies.length; i++){
        const dto = await MedicalRecordAllergyMapper.toDTO(medicalRecordAllergies[i]);
        aux.push(await this.fixDto(dto));
      }

      return aux;
    }

    private async fixDto(dto: IMedicalRecordAllergyDTO): Promise<IMedicalRecordAllergyDTO> {
      const allergy = await this.allergyRepo.getById(dto.allergy);
      dto.allergy = allergy.allergy;
      dto.doctor = await this.getDoctorName(dto.doctor);
      return dto;
    }

    private async getDoctorName(doctorId: string): Promise<string> {
      return "Doctor";
    }
}

