import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import configAux from "../../configAux.json";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import IMedicalRecordAllergyRepo from "./IRepos/IMedicalRecordAllergyRepo";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import IMedicalRecordFreeTextRepo from "./IRepos/IMedicalRecordFreeTextRepo";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import IStaffDetailsDTO from "../dto/IStaffDetailsDTO";
import http from 'http';
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import IMedicalRecordFamilyHistoryRepo from "./IRepos/IMedicalRecordFamilyHistoryRepo";
import {MedicalRecordFamilyHistoryMap} from "../mappers/MedicalRecordFamilyHistoryMapper";
import {MedicalRecordFreeTextMap} from "../mappers/MedicalRecordFreeTextMapper";
import IMedicalRecordFreeTextRepo from "./IRepos/IMedicalRecordFreeTextRepo";
import {MedicalConditionNotFoundException} from "../domain/MedicalCondition/Exceptions/MedicalConditionNotFoundException";
import {MedicalRecordConditionNotFoundException} from "../domain/MedicalRecordCondition/MedicalRecordConditionNotFoundException";
import {Code} from "../domain/Shared/code";
import {Designation} from "../domain/Shared/designation";


@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo,
        @Inject(config.repos.medicalRecordAllergy.name) private medicalRecordAllergyRepo: IMedicalRecordAllergyRepo,
        @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo,
        @Inject(config.repos.medicalRecordFreeText.name) private medicalRecordFreeTextRepo: IMedicalRecordFreeTextRepo,
        @Inject(config.repos.medicalRecordCondition.name) private medicalRecordConditionRepo: IMedicalRecordConditionRepo,
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo,
        @Inject(config.repos.medicalRecordFamilyHistory.name) private medicalRecordFamilyHistory: IMedicalRecordFamilyHistoryRepo
    ) {}

    public async createMedicalRecord(medicalRecordId:string): Promise<void> {
      const medicalProps = {};
      const medicalRecord = MedicalRecord.create(medicalProps);
      await this.medicalRecordRepo.save(medicalRecord.getValue(), medicalRecordId);
    }
    async createFamilyHistory(medicalRecordID: any, familyHistory: any) {
        const medicalRecord = this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordID);

        if (!medicalRecord) {
            throw new Error(`Medical record with ID ${medicalRecordID} not found`);
        }
        const familylist = [];
        for (const familyHistoryDTO of familyHistory) {
            console.log('familyHistoryDTO', familyHistoryDTO);
            const familyhistory = MedicalRecordFamilyHistoryMap.toDomain(familyHistoryDTO);
            console.log('familyhistory', familyhistory);
            familylist.push(familyhistory);
        }
        console.log('guardar');
        await this.medicalRecordFamilyHistory.saveFamilyHistory(medicalRecordID, familylist);
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
        console.log('Passou3');
        // Atualiza as condições no registro médico
        console.log('updatedConditions before', medicalRecord.medicalRecordConditions);
        medicalRecord.medicalRecordConditions = updatedConditions;
        console.log('updatedConditions after', medicalRecord.medicalRecordConditions);

        // Salva o registro médico atualizado
        await this.medicalRecordRepo.saveUpdate(medicalRecord, medicalRecordId);
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
      dto.allergy = allergy.designation;
      const doctor = await this.getStaffDetails(dto.doctor);
      dto.doctor = doctor.firstName + " " + doctor.lastName;
      return dto;
    }

    public async getMedicalRecordConditions(medicalRecordId: string): Promise<IMedicalRecordConditionDTO[]> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);
        
        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }
        
        const medicalRecordConditionList = await this.medicalRecordConditionRepo.getMedicalRecordConditionsWithIds(medicalRecord.props._id.toString());
        
        
        if (medicalRecordConditionList.length === 0) {
            throw new NoMedicalRecordConditionsException();
        }

        const medicalRecordConditionDTOList = [];

        for (const medicalRecordCondition of medicalRecordConditionList) {

            const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

            const condition = await this.medicalConditionRepo.getMedicalConditionByBusinessId(medicalRecordCondition.conditionId);

            const medicalConditionDTO = MedicalRecordConditionMapper.toDTO(medicalRecordCondition, condition, medicalRecord.id, staffDetailsDTO);
            
            medicalRecordConditionDTOList.push(medicalConditionDTO);
        }

        return medicalRecordConditionDTOList;
    }

    public async getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const code = Code.create(conditionCode);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByCode(code.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Code.");
        }
        

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());
        

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Code.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);
        
        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    public async getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const designation = Designation.create(conditionDesignation);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByDesignation(designation.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Designation.");
        }

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Designation.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    private async getStaffDetails(staffId: string): Promise<IStaffDetailsDTO> {
        
        const baseUrl = configAux.Backend1.URL || 'http://localhost:4000';
        
        const url = baseUrl + '/Staff';
        const urlApi = `${url}/by-id/${staffId}`;

        return new Promise((resolve, reject) => {
            http.get(urlApi, (res) => {
                let data = '';

                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {

                    try {
                        const staff: IStaffDetailsDTO = JSON.parse(data);
                        resolve(staff);
                    } catch (error) {
                        resolve(undefined);
                    }
                });
            }).on('error', (error) => {
                reject(error);
            });
        });
    }


    public async addFreeText(medicalRecord: any): Promise<any> {

        const medicalRecordDomain=MedicalRecordFreeTextMap.toDomain(medicalRecord);
        await this.medicalRecordFreeTextRepo.save(medicalRecordDomain);

    }

}



